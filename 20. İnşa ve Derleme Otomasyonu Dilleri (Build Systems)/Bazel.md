# Bazel

## Özet
Bazel; Aslen Google'ın kendi iç organizasyonu (Devasa Milyarlarca satırlık Monorepo - Tek bir Kod Deposu sitemi) için 2015 yıllarında icat ettiği (İçerideki adı Blaze'dir), sonrasında Dünyaya Açık Kaynak olarak Açılan, Make ve CMake'in o Yavaş Tek-Bilgisayarlı Yapısını Patlatarak; Kodlarınızı **Uzak Sunucularda (Bulutta) Ortaklaşa (Dağıtık/Distributed), Cache'lenmiş (Önbellekli) ve Bir Daha Asla Gereksiz Yere Tekrar Derlenmeyen Işık Hızında** Bir Yapıya Kavuşturan Çok-Dilli (Multi-Language) **Nihai Derleme Otomasyon (Build System)** Makinesidir.

## Nedir ve Ne İşe Yarar?
Google'ın kod tabanı (Search, YouTube, Android Arka Planı) 2 Milyar satır koddur (Tek Bir Büyük Klasördür/MonoRepo). Eğer orada `make` ile Derleme (Compile) çalıştırısanız; o 2 milyar kodun derlenip Exe'ye dönmesi Aylar sürerdi!

Google mühendisleri BAZEL'i üretti. Bazel der ki: **"Bir Kodu (Örneğin 'A' fonksiyonunu) Evrende Yalnızca 1 Kere Derle! Çıkan Sonucu (Hash) Google Bulutuna (Remote Cache) Kaydet. Eğer başka binlerce yazılımcı Kendi Laptopunda O klasörü derlerse; Kodları baştan derleme! Git Buluttan (Saniyede İndirip) Adama Sun (Hermetic Build)!"** 
Bu sayede Saatler Süren C++, Java, Go Kod derlemeleri; Yazılımcılar arasında "Ortaklaşa Kütüphane Zekası" İle **Milisaniyelere** düşmüştür.

**Ne İşe Yarar?**
* **Çoklu-Dil Desteği (Multi-Language):** CMake Sadece C++ odaklıdır. Ama Sizin Şirketinizde Önyüz (Angular/Typescipt), ArkaYüz (Go/Java) Ve Çekirdek (C++) hep birlikte 1 Klasörde Yatıyorsa; Bunların Hepsini Uyum ve Hiyerarşi içinde Aynanda Tek bir Komutla (`bazel build //...`) Pürüzsüze Derleyiciye sokan TEK Patron Bazel'dir.
* **Hermetik (Dışarıya Kapalı) Çevrelere Karşı:** Bir bilgisayarda Java'nın X sürümü, diğerinde Y Sürümü olsa bile Bazel "Kendi Göbek Bağını Kendi keser", İçeride İnternetten İstediği Java'ları İndirip Bütün Arkadaşlarda %100 AYNI (Deterministic) Mimaride Kodu Yaratmayı Garantiler(Zırhlar). Hata Şansı Sıfırdır!

## Dilin Mantığı ve Kod Yapısı
Sözdizimi ve Yazım dili (Starlark isminde) tamamen **Python'un Klonudur!** Ama O bir Yılan (Python Scripti) Değildir! Çaktırmadan Döngüleri vb kısıtlanmış "Salt Fonksiyonel ve Deterministik (Asla farklı Sonuç vermeyen)" Bir Ayar dilidir. Klasörlerin içine `BUILD` Veya `BUILD.bazel` İsminde Dosyalar açılarak Hiyerarşi Kurulur (Workspace/Ağaç Şeması).

### Örnek Bir Bazel Kodu: C++ Ve Kendi Kütüphanemizi Birleştiren (Monorepo) Pythonik Şaheser
Bir `BUILD` dosyasını açıp klasörleri Derleme Hedefi (Target) Olarak Belirleme:

```python
# BU BIR BAZEL DOSYASIDIR (Yazim Sekli Python'u Andırır - Adı Starlark'tir)

# 1. KUTUPHANE (LIBRARY) YARATMA HEDEFI (TARGET)
# Burada Benim "matematik.cc" İsmindeki C++ Dosyami DİĞER Yazılımcıların Kullanabileceği (Include) Bir Modüle Çeviririz
cc_library(
    name = "kral_fonksiyonlar",              # Disaridan Bu Isimle Çağırırız
    srcs = ["matematik.cc"],                 # Icinde Yatan Gerçek C++ Kodu
    hdrs = ["matematik.h"],                  # Baslik (API) imzamiz!
    visibility = ["//main:__pkg__"],         # GUVENLIK: Bu Kutuphaneyi SADECE "main" Klasorundeki Arkadasim Kullanabilir. Baska Klasor(Youtube Klasörü) Bunu ÇALAMAZ! (Dependency Control)
)


# 2. ANA PROGRAM(EXE) YARATMA HEDEFI
# Simdi Oyunumuzu Veya Uygulamayı Ayağa Kaldıracağım!
cc_binary(
    name = "benim_uygulamam",                # Cikacak Sonucun/Programin Adi
    srcs = ["merhaba_dunya.cc"],             # Kullanilan (Main İçeren) Dosya
    
    # 3. BAGLANTILAR LİSTESI(Dependencies) - En Buyuk Sihir Burasi!
    deps = [
        # Yukarıdaki Kendi Kutuphanemize(kral_fonksiyonlar) Bagla/Kopru At !!
        ":kral_fonksiyonlar",               
        
        # Ayni Zamanda DISARIDAKİ DİĞER KLASÖRDEKİ Abseil C++ (Google'ın Açık Kaynak Kütüphanesi)'nin String Modülünü De Benim İçime Mıhla/Include Et:
        "@com_google_absl//absl/strings",    
    ],
)
```
Bu sistemde Bir adam Terminalde `bazel build //:benim_uygulamam` Yazar. Bazel; İnternetten Gider Abseil Kütüphanesini(Google API) bulur İndirir Kendi hafizasında Derler. Sizin Matematigi Çözer.. Hepsini Zımbalar. Ertesi Gün Sadece `merhaba.cc` İçindeki 1 HARFİ BİLE Değiştirirseniz; Bazel C++ Kütüphanelerini (Abseil ve Matematiği) **YENİDEN DERLEMEZ! (Cünkü Degismemistir)**. Direkt Buluttan/Cache'inden Eski Derlenmiş Parçayı(Binary) Saniyesinde Kopyalar; Total derleme 40 Dakikadan Saniyelere düşer.

## Kimler Kullanır?
* Binlerce Yazılımcının (ve Yüzlerce farklı programlama dilinin) Aynı klasörde Cirit attığı, Milyonlarca satırlık Mimarilere (Monorepo) Sahip Dev Teknoloji **(Google, Uber, Pinterest, SpaceX) Takımı / Mühendis Geliştirme (Developer Productivity)** Mimarları.
* Çoklu platform hedeflemeyen Ama Safkan (Build Süreleri İnsani Çıldırtan, Kafe Molası verdiren) Acıyı Tadan Tüm Sistem Şeflerinin Çözüm İlacıdır.. Sıradan, kendi başına kod yazan Küçük Çaplı Indie geliştiriciler için Makinenin Kurulum Eziyeti Çok Büyük (Overkill) olacağından Çoğu Kişi Hala CMake Kullanır. BAZEL, Kralların Fabrikasıdır.
