# Scratch

## Özet
Scratch; 2007 yılında MIT Media Lab tarafından geliştirilen, kod yazmayı "Sürükle-Bırak" (Drag and Drop) mantığıyla Lego bloklarına dönüştüren, 8-16 yaş arası çocuklara (ve programlamaya yeni başlayan her yaştan insana) algoritmik düşünceyi öğretmek amacıyla tasarlanmış, dünyanın gelmiş geçmiş en popüler **Görsel/Blok Tabanlı (Visual-Block-Based)** eğitim dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda ve 2000'lerin başında çocuklara kodlama öğretmek zordu. Algoritma (Döngü, Eğer-İse) mantığını kavramadan önce, noktalı virgülleri unutmak, yazım hataları (Syntax Errors) yapmak çocukları programlamadan soğutuyordu. 
MIT Media Lab, "Eğer kodları lego parçalarına çevirirsek, çocuklar mantık hatası yapsa bile yazım hatası yapmazlar" felsefesiyle Scratch'i yarattı. Bir kedi karakterini (Mascot) hareket ettirmek, IF bloklarını iç içe geçirmek sadece fareyi sürüklemekten ibaretti. 

**Ne İşe Yarar?**
* **Temel Algoritma ve Bilgisayarca Düşünme (Computational Thinking):** Scratch ile döngülerin (Loop), karar yapılarının (If/Else) ve değişkenlerin (Variables) ne işe yaradığı ekrandaki kedinin (Sprite) zıplamasıyla anında görselleştirilir.
* **Oyun ve Animasyon Yapımı:** Milyonlarca çocuk Scratch web sitesinde kendi PAC-MAN klonlarını, labirent oyunlarını veya interaktif hikayelerini tasarlayarak yayınlar.

## Dilin Mantığı ve Kod Yapısı
Geleneksel bir "Sözdizimi (Syntax)" harf zincirine sahip değildir. Kodlar, renk ve şekillerine göre ayrılmış bloklardır:
- **Sarı Bloklar (Olaylar):** "Yeşil Bayrağa tıklandığında" (Programın başlama anı).
- **Mavi Bloklar (Hareket):** "10 adım git", "15 derece dön".
- **Turuncu Bloklar (Kontrol):** "Sürekli Tekrarla" (Forever Loop), "Eğer ... ise" (If-Then condition).

Çocuklar bu blokları alt alta yap-boz gibi birleştirir. Bir bloğun çıkıntısı, diğerinin girintisine uymuyorsa oraya takılamaz (Böylece Type Mismatch / Tip Hatalarını görsel olarak engeller).

**Örnek İşleyiş (Sembolik Olarak):**
Python: `while True: x = x + 10; print(x)`
Scratch: `[Sürekli Tekrarla] -> İçine koy [X konumunu 10 değiştir]` (Fare ile sürüklenir).

### Örnek Bir Scratch Mantığı: Zıplayan Kedi Oyunu
Ekranda bir kedi figürü vardır ve biz Boşluk (Space) tuşuna basınca havaya zıplayıp düşmesini komutlarız (Metin dili değil, Blok felsefesi).

```text
[OLAY BLOKU]: "Boşluk (Space) tuşuna basılınca"
|
|-- [KONTROL BLOKU]: "10 defa tekrarla"
|   |
|   |-- [HAREKET BLOKU]: "Y konumunu 10 değiştir"
|       (Kedi havaya yukselir)
|
|-- [KONTROL BLOKU]: "10 defa tekrarla"
|   |
|   |-- [HAREKET BLOKU]: "Y konumunu -10 değiştir"
|       (Kedi yercekimiyle geriye duser)

[OLAY BLOKU]: "Yeşil Bayrağa Tıklanınca"
|
|-- [KONTROL BLOKU]: "Sürekli Tekrarla"
|   |
|   |-- [GÖRÜNÜM BLOKU]: "Sonraki kostüme geç"
|   |-- [HAREKET BLOKU]: "10 adım git"
|   |-- [HAREKET BLOKU]: "Kenara geldiysen sek"
    (Kedi saga sola yurur animasyonlu!)
```
Bu kadar basittir. Değişken yaratmak, Can puanı tutmak, Puanı ekrana yazdırmak hepsi Lego takmak gibidir. Çocuklar "Syntax Error on Line 15" diye bir kâbus görmezler.

## Kimler Kullanır?
* Dünyadaki tüm İlkokul ve Ortaokulların **Bilişim Teknolojileri / Yazılım Öğretmenleri**. Algoritma eğitiminin giriş kapısıdır (Dünyada 100 Milyondan fazla kayılı kullanıcısı vardır).
* Kodlamaya meraklı 8-16 yaş arası çocuklar.
* Daha sonra kendi oyununu yapmak isteyen Maker/Robotik heveslileri (Scratch'ın blok bağlantıları mBot veya Arduino gibi donanımların da çocuklara öğretilmesinde kullanılır - mBlock vs). Kesinlikle Ticari/Endüstriyel bir dil değildir, ancak eğitim evreninin tartışmasız kralıdır.
