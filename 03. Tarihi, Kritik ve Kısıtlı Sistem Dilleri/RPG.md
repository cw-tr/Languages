# RPG (Report Program Generator)

## Özet
RPG (Report Program Generator); 1959 yılında dev IBM tarafından, şirketlerin Punch-Card (Delikli Kartlar - Bilgisayarlardan önceki karton otomasyonlar) üzerinden okudukları verileri alıp yazıcıdan şık, temiz ve hizalanmış sütünlardan oluşan "Muhasebe Kağıdı/Raporlar" basabilmek amacıyla tasarlanan, ve çok ilginç biçimde devasa mutasyonlar geçirerek (Bugün RPG IV/ILE RPG) modern IBM i (AS/400) kurumsal sunucularının omurgası olarak hâlâ çılgınca kullanılan bir Kurumsal-Sütunlu dildir.

## Nedir ve Ne İşe Yarar?
1960'larda ve 70'lerde Fabrikalar, Lojistik ambarlar ya da Maaş dağıtan muhasebeler "Ekrana bir Menü / Web Sitesi" çıkarma derdinde DEĞİLDİ. Tek aranan şey; Günün sonunda binlerce depocudan gelen sayıları bilgisayarın (IBM Mainframe) veritabanına atıp bunu "Line-Printer" (Dev Nokta Vuruşlu Yazıcılar) larına kusursuz Sütunlarla Hizalayarak "Rapor" çıkarabilmekti.

IBM RPG'yi tam bunun için kurdu: Veritabanı (DB2 vb) bağla -> Veriyi İşle -> Kağıda Şık Karakterlerle Yazdır! 

**Ne İşe Yarar?**
* **AS/400 IBM i Ekosisteminin Kanı:** Bir şirkette eğer siyah arka plana yeşil veya beyaz metin dökülen bir Terminal Teyp Ekrani - Mainframe (eski nesil IBM Power Systems) görüyorsanız ve firma koca bir üretim (Manufacturing) ya da Banka/Lojistik zinciriyse (Örn: Costco, dev tekstil devleri), arkasında koşturulan Programın %99 ihtimalle RPG (çoğu zaman RPG IV) olması kesindir.
* COBOL'den Milyon kat daha spesifik ve IBM'in donanımına KÖKÜNDEN zincirlenmiş tam entegre veritabanı işleyen bir otomasyondur.

## Dilin Mantığı ve Kod Yapısı
Evrendeki hiçbir popüler dil RPG gibi kodlanmaz, çünkü RPG **"Position-Based - Pozisyona Bağlı (Sütun Odaklı)"** bir dildi. Siz kod yazarken metni sola ya da sağa boşluk (Space) koyarak keyfinize göre KAYDIRAMAZSINIZ! Ekranda haritalanmış görünmez 80-Karakterlik veya 100-Karakterlik Sütun duvarları vardır. 

Örneğin, bir Kararın (Formülün), 6. Sütun ile 10. Sütun arasına TAM HIZASINDA denk gelmesi emredilirdi. Lakin 2000'lerden sonra çıkan Free-Format RPG (RPG IV / ILE RPG) bu korkunç Sütun(Hiza) zorunluluğunu kırıp kodu C ve Java gibi serbest, akıcı bloklara (Free-Format `**FREE`) dönüştürerek devrim yapmıştır.

**Örnek İşleyiş (Sembolik Olarak):**
Geleneksel (Eski) RPG'de Her satırın başına (Spesifikasyon Kodu) F(Dosya), D(Değişken), C(Hesaplama) ve O(Çıktı Rapor) gibi O satırın ne işe yaradığını söyleyen tekil bir harf kodlanır. Böylece derleyici Sütun kulesinden okur. Bu yüzden okuması "Bulmacadan/Şifrelemeden" farksızdır.

### Örnek Bir RPG Kodu: Modern / Free-Format (Özgür Hiza) RPG IV
Sütun fantezisini bırakıp modernleştirilmiş, ancak ruhunu hala IBM'in AS/400 terminalindeki DB(Veritabanı) süzme şablonunda taşıyan "Bir Müşteri Dosyasını Oku ve Ekrana Rapor Bas" mantığı:

```rpgle
**FREE
// İŞTE MUCİZE ! İlk satıra '**FREE' kelimesi yazılarak IBM derleyicisine
// "Eski RPG gibi Kodumda Sütun/Hiza bekleme, C/Java gibi özgür okuyacaksın!" emri ulaştırılır.
// // ile baslanan çift slash yorum manasındadır.

// 1. ADIM (F - File/Dosya Spesifikasyonları yerine) Modern Dosya Tanimlamasi:
// MUSTERILER ismindeki IBM (AS/400 - DB2) Sistem tablosunu DISK'ten Sırasiyla Oku(*INPUT)
DCL-F Musteriler DISK(*EXT) USROPN KEYED; 

// 2. ADIM (D - Degisken Spesifikasyonları):
// Hafizada, Müsterinin Borcunu tutacagimiz Packed-Decimal (Para formatında 7 Hane, 2 Virgüllu) tip:
DCL-S KalanBorc PACKED(7:2) INZ(0); 

// Ekrana Yazi Basilma (Rapor) Zirvesi 
// Program ASIL Buradan "Calismaya / Engine" donmeye baslar!
DSPLY '---- GUNLUK MUSTERI BAKIYE RAPORU ----';

// Dosyayi (Musteri.db) Hafizaya Ac ve Okumaya Basla:
OPEN Musteriler;

// READ Koduyla veritabanindan tek kayit Cek (Dogrudan dilin Kalbine Gomulu DB entegrasyonu!)
READ Musteriler;

// DONGU (Loop): Tablodaki satirlar Sona Erene Kadar (DOW NOT %EOF) Oku
DOW NOT %EOF(Musteriler);

    // Veritabanındaki "BAKIYE" sutunun Sifirdan buyuk olan Alacaklilari bul:
    IF Bakiye > 0;
        // Bakiye, Borç Hanesine Ekleniyor:
        KalanBorc = KalanBorc + Bakiye;
        
        // Klasik RPG Raporlayicisi: Karakterleri (Musteri Adini ve Bakiyesini) ekrana Raporla (DSPLY):
        DSPLY (Must_Adi + ' ---> Borc Miktari: ' + %CHAR(Bakiye));
    ENDIF;
    
    // Sonraki Satırı Oku
    READ Katalog;
ENDDO;

// Veritabanini Kapat. Rapor Bitti!
CLOSE Musteriler;
DSPLY ('TOPLAM FIRMA ALACAGI: ' + %CHAR(KalanBorc));

// LRM Kapatilmasi (IBM Mainframe Sistemine Programin bittigini soylemek, bellegi temizlemek *INLR)
*INLR = *ON; 
```

Dünyanın yarısını çeviren eski nesil COBOL kodları neyse, Milyarca Lojistik ve Satın-alma depolama terminallerini döndüren (Ama halk arasında asla bilinmeyen) "Ninja/Hayalet" yazılım ve makine uyumu da RPG'dir.

## Kimler Kullanır?
* Evinde oturan veya "Mobil uygulama" yapan kimse kullanmaz. 
* Dünyada hala "AS/400 (Şimdi IBM i Series deniyor)" adındaki Mainframe devasa donanımları şirketlerinde ısrarla koşturan (Çünkü hiç donmuyor, çökmüyor, 30 yıllık Legacy kodu saniyesinde okuyor); büyük Lojistik filoları, Zincir Marketler, Dağıtım ağı yöneten Enterprise firmalar bünyelerinde maaşlı **"RPG IV Developer"** istihdam eder. 
* Gençler bu efsane dilden kaçtığı için, günümüzde iyi bir RPG yazılımcısı ABD ve Avrupa'da inanılmaz yüksek dolar karşılığı transfer rakamlarıyla kapışılır.
