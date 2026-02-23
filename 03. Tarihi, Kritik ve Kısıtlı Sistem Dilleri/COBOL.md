# COBOL

## Özet
COBOL (Common Business-Oriented Language); 1959'da icat edilmiş, okunuş itibarıyla neredeyse tamamen düz İngilizce paragraflara benzeyen, dünyadaki finans-muhasebe ve trilyonlarca dolarlık bankacılık işlemlerinin hala gizli kahramanı olan devasa bir kurumsal dildir.

## Nedir ve Ne İşe Yarar?
Bilgisayarların sadece mühendisler ya da matematikçiler (Fortran) için değil, muhasebeciler ve işletme yöneticileri tarafından da kullanabilmesi hayaliyle oluşturulmuştur. Grace Hopper liderliğindeki ekibin eseridir. Dilin tasarımında "bir yöneticinin kodlara baktığında okuyup anlayabilmesi" şartı (Self-Documenting) koşulmuştur.

**Ne İşe Yarar?**
* **Aşırı Yüksek Hacimli Kurumsal Veri İşleme (Batch Processing):** Geceleri bankaların devasa anaçatı (Mainframe) bilgisayarlarında milyonlarca müşterinin kredi kartı ekstremize işlemlerini, faiz hesaplarını aynı anda, hata payı sıfır olacak bir keskinlikle işler.
* **Katı Sabit Noktalı Aritmetik (Fixed-Point Math):** Modern diller ondalıklı sayılarda (0.1 + 0.2 = 0.300000004 gibi) ufak kayıplar yaşarken, COBOL parayı tamı tamına kuruşu kuruşuna sayacak özel bir veri işleme motoruna sahiptir; bankacılık hatasını kabul etmez.

## Dilin Mantığı ve Kod Yapısı
COBOL, programları `Bölümlere (Divisions)` ayırır. Tıpkı bir şirketin departmanları gibidir: 
* `IDENTIFICATION DIVISION` (Kodun Künyesi - Yazar, Tarih vb.)
* `ENVIRONMENT DIVISION` (Çalışılacak Donanım / Klasör Yolları)
* `DATA DIVISION` (Değişkenler, Veritabanı yapısı)
* `PROCEDURE DIVISION` (Algoritmalar ve Komutlar)

Komutlarda süslü parantezler `{` yerine tamamen İngilizce gramer hakimdir. Satırların sonunda tıpkı normal bir cümledeki gibi nokta `.` konur. Bir sayıyı değişkene atamak için `=` işareti yerine `ADD 5 TO BANKA-HESABI` şeklinde cümle kurulur.

**Örnek İşleyiş (Sembolik Olarak):**
Eğer günümüzdeki C veya Python dilinde "Maaştan vergi oranını çıkarıp Net Maaşı bul" demek isteseydik `netMaas = brütMaas - (brütMaas * 0.20)` gibi matematiksel simgeler kullanırdık. COBOL'da ise kelimenin tam anlamıyla "Brüt Maaş'ı 0.20 ile çarp Vergi'ye at, sonra Vergi'yi Brüt Maaş'tan çıkar Net Maaş'a at" şeklinde bir kompozisyon kurulur.

### Örnek Bir COBOL Kodu: İşlem ve Para Aktarımı
Okuması tamamen doğal İngilizce roman gibi gelen, iki şirket arası para transferi yapan, veriyi işleyen tipik (eski usul) bir şirket içi `Procedure` parçası:

```cobol
       * COBOL'da bu sütundan başlayan satırlar Yıldız ile (*) yorum satırıdır.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARA-TRANSFERI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       * Burada "9(5)" demek en fazla 5 haneli tam sayı, "V99" ise 2 haneli kuruş demek.
       01 MUS-HESAP-BAKIYESI  PIC 9(5)V99 VALUE 25400.50.
       01 AKTARILACAK-TUTAR   PIC 9(5)V99 VALUE 01250.25.

       PROCEDURE DIVISION.
       ANA-ISLEM.
           * Şirket içi matematiksel talimatlar:
           SUBTRACT AKTARILACAK-TUTAR FROM MUS-HESAP-BAKIYESI.

           * Durumu Kontrol Et:
           IF MUS-HESAP-BAKIYESI < 0
               DISPLAY "Hata: Yetersiz Bakiye! Islem Iptal Edildi."
           ELSE
               DISPLAY "Transfer Basarili. Kalan Bakiyeniz: " MUS-HESAP-BAKIYESI
           END-IF.

           * Programı tamamen sonlandırıp, sistemi Anaçatıya teslim et:
           STOP RUN.
```
Yukarıdaki kod, 1960'lardan beri çalışan ve bugün dahi IBM'in z/OS ana bilgisayarlarında saniyede binlerce kez koşturulan dev bankacılık komut setinin minyatür prototipidir.

## Kimler Kullanır?
* Dünyadaki tüm büyük Bankalar, Sigorta Şirketleri ve Havayolu rezervasyon (Amadeus vb.) biletleme sistemlerinin ana yongaları. Günümüzde her gün ATM'den para çektiğinizde arkadaki %80 ihtimalle COBOL tabanlı bir hesaptır.
* Devlet kurumlarının (Örn: IRS - ABD Vergi Dairesi) sosyal güvenlik veya vergi takip altyapısında terabaytlarca düz metin/veri işleyen departmanlar.
* Sistemlerin modern dillere (Java vb.) geçirilemeyecek kadar aşırı büyük, riskli veya karmaşık olduğu (Legacy Systems/Miras Sistemler) kurumlardaki çok yüksek maaşlı ve deneyimli bakım/onarım mühendisleri.
