# Piet

## Özet
Piet; David Morgan-Mar (Chef dilinin de yaratıcısı olan deha) tarafından icat edilmiş, kod satırlarının ASCII/harf yerine **Görüntü (Bitmap/Pikseller)** olduğu, Hollandalı soyut ve modern sanat ressamı "Piet Mondrian"dan ilham alan, program yazdığınızda ortaya "Muazzam bir Soyut Tablo / Sanat Eseri" çıkan evrendeki **Tek Görsel-Renk Eksenli Programlama Dilidir**.

## Nedir ve Ne İşe Yarar?
1900'lerin başında Ressam Piet Mondrian, sadece Kırmızı, Sarı, Mavi Kareler ve siyah kalın çizgilerle muazzam sanatsal tablolar çiziyordu. 
Morgan-Mar şunu düşündü: "Eğer bir makine kodu harflerden değil de Renk Değişimlerinden okusaydı nasıl olurdu?". Piet dili icat edildi.

Piet dilinde "A" veya "İF" yazmazsınız. Photoshop (Veya MS Paint) açarsınız, pikselleri yan yana boyarsınız. Derleyici (Interpreter) sizin .PNG/.GIF uzantılı o Renkli Resminizi içine atıldığında, Sol-Üst pikselden sağa doğru yılan (Pointer) gibi yürümeye başlar. Bir renkten diğerine (Örn: Sarıdan Koyu Kırmızıya) Geçiş sırasındaki **Kontrast ve Ton (Hue)** matematiği, makineye Push/Pop emrini hissettirir!

**Ne İşe Yarar?**
* **Piyasa İşi:** Tamamen sıfır. Sadece sanat (Art) ve Yazılım (Science) fütürizminin kesiştiği tek mutlak noktadır.
* **Kriptografi Ve Saklama:** Duvarınıza astığınız şık, soyut, modern bir tablonun (Pixel Art veya Mondrian dizilimi); aslında tarayıcıdan (Scanner) geçirilip Derleyiciye Sokulduğunda Ekrana "Güvenlik Şifresi" basan çalışan bir Linux Yazılımı (Uygulama) olmasını sağlar (Harika bir James Bond tarzı Steno).

## Dilin Mantığı ve Kod Yapısı
Dilde toplam (Aktif kullanılan) **20 Tane Renk** vardır. Bunlar aydınlık (Light), Normal ve Koyu (Dark) olmak üzere Kırmızı, Sarı, Yeşil, Camgöbeği, Mavi ve Macenta (Pembe) matrisleridir. Extradan Siyah (Duvar/Sınan) ve Beyaz (Atla/Pas) vardır.

- **Pointer Gezintisi:** Motor görselin En Sol-Üst Pikselinden başlar. Renk blokları arasında gezer. 
- **Komutların Şifresi:** Diyelim ki Kırmızı kutudan Çıktı, Koyu Yeşil kutuya girdi! Derleyici bunu görür, Renk çarkında "Kaç adım dönmüş, Ne kadar Koyulaşmış" diye Çark Matematiği yapar. Bu değişim mesafesi ÖRN: "Topla (Add)" demektir! Veya "Ekrana Bas (Print/Out)" demektir.
- **Siyah Bloklar (Duvarlar):** Siyah renge çarpan Pointer (Kod Akışı) Duvardan sekip yön değiştirir (Yani Saat yönüne (Sağa veya Aşağı filan) ilerlemeye başlar). Siyah bloklar IF (Eğer/Yönlendir) görevi görür.

**Örnek İşleyiş:**
Kodu "Yazmak" yerine Paint'te Boyalarsınız (Veya Piet Özel IDE'lerinde Tasarlarsınız).

### Örnek Bir Piet Kodunun "Anlamı"
(Resim Çizemediğimiz İçin O Mantığın Piksel Açıklaması) Ekrana sadece **"Pi" (3.14...) basan o Sanat Eseri Tablosunun Akışı:**

```text
(GOZUNUZUN ONUNDE MODERN BIR PIKSEL-TABLOSU CANLANDIRIN)

[Acik Kirmizi Blok - 5 Piksel] -> [Koyu Kirmizi Blok]
(Açıklama: Açık renkten Koyu Renge 1 Ton inis: "Stack'e 5 Eklesin" (Push 5))

-> [Siyah Sınır Çizgisi]
(Pointer siyaha carpti. Yönünü Asagiyaa (Aşağıya Dogru inmeye) cevirdi!)

[Aşağıdaki Koyu Mavi Blok - 3 Piksel]
(Renk Çarkında Kırmizdan Maviye 2 Çeyrek, Koyuluk Sabit. Bu "Çarpma İşlemi (Multiply)" demek)

[Koyu Mavi'den -> Normal Magenta'ya Gecis]
(Ekrana Sayısal Olarka OUT(Baski) Ver!)

(Derleyici bu renk dansini okudugu an Terminale : 3.14  Sayisini Basar)
```

Eğer bir Pikseli yanlışlıkla siyah boyarsanız, Program (Kod Yılanı) içinden çıkılmaz bir Sonsuz Döngüye (Siyah duvarlar arasında pinpon topu gibi seker) girer. (Bug dediğimiz şey aslında fırça lekesidir).

## Kimler Kullanır?
* Piksel Rengi üzerinden Yazılıma Felsefi/Sanatsal tepki veren **Pixel-Art (Piksel Sanatçıları) ve Algoritmik Ressamlar**.
* Herhangi bir teknoloji Müzesinde Veya Silikon Vadisine giriş Tablolarında; O duvarda asılı duran "Mondrian Tablosunun" aslında Arka planda Fibonacci Sayılarını hesaplayan kusursuz bir EXE Dosyası olduğunu kimse bilemez. Şeffaflık (Obfuscation) krallığıdır.
