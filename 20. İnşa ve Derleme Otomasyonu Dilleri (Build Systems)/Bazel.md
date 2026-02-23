# Bazel

## Özet
Bazel; Aslen Google'ın kendi iç organizasyonu (Devasa Milyarlarca satırlık Monorepo - Tek bir Kod Deposu sitemi) için 2015 yıllarında icat ettiği (İçerideki adı Blaze dir), sonrasında Dünyaya Açık Kaynak olarak Açılan, Make ve CMake'in o Yavaş Tek-Bilgisayarlı Yapısını Patlatarak; Kodlarınızı **Uzak Sunucularda (Bulutta) Ortaklaşa (Dağıtık/Distributed), Cache'lenmiz (Önbellekli) ve Bir Daha Asla Gereksiz Yere Tekrar Derlenmeyen Işık Hızında** Bir Yapıya Kavuşturan Çok-Dilli (Multi-Language) **Nihai Derleme Otomasyon (Build System)** Makinesidir.

## Nedir ve Ne İşe Yarar?
Google'ın kod tabanı (Search, YouTube, Android Arka Planı) 2 Milyar satır koddur (Tek Bir Büyük Klasördür/MonoRepo). Eğer orada `make` ile Derleme (Compile) çalıştırısanız; o 2 milyar kodun derlenip Exe ye dönmesi Aylar sürerdi!

Google mühendisleri BAZEL'i üretti. Bazel der ki: **"Bir Kodu (Örneğin 'A' fonksiyonunu) Evrende Yalnızca 1 Kere Derle! Çıkan Sonucu (Hash) Google Bulutuna (Remote Cache) Kaydet. Ege başka binlerce yazılımcı Kendi Laptobunda O klasörü derlerse; Kodları baştan derleme! Giy Buluttan (Saniyede İndirip) Adama Sun (Hermetic Build)!"** 
Bu sayede Saatler Süren C++, Java, Go Kod derlemeleri; Yazılımcılar arasında "Ortaklaşa Kütüphane Zekası" İle **Milisaniyelere** düşmüştür.

**Ne İşe Yarar?**
* **Çoklu-Dil Desteği (Multi-Language):** CMake Sadece C++ odaklıdır. Ama Sizin Şirketinizde Önyüz (Angular/Typescipt), ArkaYüz (Go/Java) Ve Çekirdek (C++) hep birlikte 1 Klasörde Yatıyorsa; Bunların Hepsini Uyum ve Hiyerarşi içinde Aynanda Tek bir Komutla (`bazel build //...`) Pürüzsüze Derleyiciye sokan TEK Patron Bazel'dir.
* **Hermetik (Dışarıya Kapalı) Çevrelre Karşı:** Bir bilgisayarda Java'nın X sürümü, diğerinde Y Sürümü olsa bile Bazel "Kendi Göbek Bağını Kendi keser", İçeride İnterneten İstediği Javalır İndirip Bütün Arkadaşalarda %100 AYNI (Determenistic) Mimaride Kodu Yaratmayı Garantiler(Zırhlaar). Hata Şansı Sıfırdır!

## Dilin Mantığı ve Kod Yapısı
Sözdizimi ve Yazım dili (Starlark isminde) tamamen **Python'un Klonudur!** Ama O bir Yılan (Python Scripti) Değildir! Çaktırmadan Döngüleri vb kısıtlanmış "Salt Fonksiyonel ve Determinestrik (Asla farklı Sonç vermeyn)" Bir Ayar dilidir. Klasörlerin içine `BUILD` Veya `BUILD.bazel` İsminde Dosyalar açılarak Hiyerarşi Kurulur (Workspace/Ağaç Şeması).

### Örnek Bir Bazel Kodu: C++ Ve Kendi Kütüphanemizi Birleştien (Monorepo) Pythonik Şaheser
Bir `BUILD` dosyasını açıp klasörleri Derleme Hedefi (Target) Olarak Belirleme:

```python
# BU BIR BAZEL DOSYASIDIR (Yazim Sekli Python'u Andırır - Adı Starlark'tir)

# 1. KUTUPHANE (LIBRARY) YARATMA HEDEFI (TARGET)
# Burada Benim "matematik.cc" İsmindeki C++ Dosyami DİĞER Yazılımcıların Kullanabiieegıi (Include) Bir Modüle CEvirriz
cc_library(
    name = "kral_fonksiyonlar",              # Disaridan Bu Isimle Cagirilziz
    srcs = ["matematik.cc"],                 # Icinde Yatan Gercev C++ Kodu
    hdrs = ["matematik.h"],                  # Baslik (API) imzamiz!
    visibility = ["//main:__pkg__"],         # GUVENLIK: Bu Kutuphaneyi SADECE "main" Klasorundeki Arkadasim Kullancailir. Baska Klasor(Youtube Klassoru) Bunu CALAAMAZ! (Dependency Control)
)


# 2. ANA PROGRAM(EXE) YARATMA HEDEFI
# Simdi Oyunumuzu Veya Uygulamyai Aaya Kaldiracahim!
cc_binary(
    name = "benim_uygulamam",                # Cikacak Sonucun/Programin Adi
    srcs = ["merhaba_dunya.cc"],             # Kullanilan (Main Icern) Dosya
    
    # 3. BAGLANTILAR LİSTESI(Dependencies) - En Buyuk Sihir Burasi!
    deps = [
        # Yuaridai Kendi Kutuphanemize(kral_fonksiyonlar) Bagla/Kopru At !!
        ":kral_fonksiyonlar",               
        
        # Ayni Zamanda DISARIDAKİ DİĞERKLASÖRDEKİ Abseil C++ (Google'İn Acikkaynak Kütüphanesi)'nin String Modülünü De Benim Icime Mıhla/İncelude Et:
        "@com_google_absl//absl/strings",    
    ],
)
```
Bu sistemde Bir adam Terminalde `bazel build //:benim_uygulamam` YAzar. Bazel; İnternetten Gider Abseil Kütüphanesini(Google API) bulur İndirir Kendi hafizasında Derler. Sizin Matematigi Çözer.. Hepsini Zımbalar. Ertesi Gün Sadece `merhaba.cc` İçindeki 1 HARFİ BİLE Değiştirirseniz; Bazel C++ Kutuphanelrrini (Abseil ve Matematiği) **YENİDEN DERLEMEZ! (Cünkü Degismemistir)**. Direkt Buluttan/Cache'inden Oeski Derlenmiş Parçayı(Bİnary) Saniyesinde Kopyaalar; Total derleme 40 Dakikadsan Saniyelere düşer.

## Kimler Kullanır?
* Binlerce Yazılımcının (ve Yüzlercr farklı programlama dilinin) Aynı klasörde Cirit attığı, Milyonlarca satırlık Mimarilere (Monorepo) Sahip Dev Teknoloji **(Google, Uber, Pinterest, SpaceX) Takımı / Mühendis Geliştirme (Developer Productivity)** Mimarları.
* Çoklu platform hedeflemeyen Ama Safkan (Build Sureleri İnsani Cildittan, Kafe Molası verdiren) Acıyı Tadan Tüm Sistem Şeflerinin CÖzüm İlacıdır.. Sıradan, kendi başına kod yazan Küçük Caplı Indie geliştiriciler için Makinenin Kurulum Eziyeti Çok Büyük (Overkill) olacağından Çoğu Kişi Hala CMake Kullanır. BAZEL, Kralların Fabrikkasıdır.
