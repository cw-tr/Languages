# ABAP

## Özet
ABAP (Advanced Business Application Programming); 1980'lerde Alman yazılım devi **SAP** tarafından, devasa holdinglerin milyarlarca dolarlık Lojistik, Muhasebe ve İnsan Kaynakları sistemlerini (ERP - Kurumsal Kaynak Planlaması) tek merkezden koda dökmek ve SAP ürünlerini "özelleştirmek" amacıyla yaratılmış, mülkiyetli (Proprietary - Kapalı Kaynak) Kurumsal İş Programlama Dilidir.

## Nedir ve Ne İşe Yarar?
Eğer bir "Oyun" veya "E-Ticaret Sitesi" yapacaksanız ABAP kullanmazsınız; dünyada kimse kişisel projesi için ABAP indirip yazmaz. ABAP, evrendeki devasa şirketlerin (Coca-Cola, Ford, Beko, THY) tüm fabrikasındaki çarkların dönüşünü, gümrükten geçen tırların vergisini, batan bütçesini kontrol eden milyon dolarlık **SAP Altyapı Sistemlerinin Dilidir**. 

SAP, şirketlere devasa ana bir yazılım satar. Ancak her şirketin kendi kanunları, kendine özel fatura hesaplama kuralları ve vergi dairesi vardır. Şirketler bünyelerine "ABAP Danışmanı / Geliştiricisi" işe alırlar. Bu uzmanlar, SAP'nin standart paketine girip şirkete özel (Custom) veritabanı dökümleri, fatura çıkarma fonksiyonları ve UI raporları kodlarlar. 

**Ne İşe Yarar?**
* **Dev Şirketlerin Omurgası (ERP Geliştirme):** Bir fabrikada yeni sipariş girildiğinde stokun azalması, paranın bilançodan düşmesi ve depoya üretim sinyali gitmesi tamamen SAP içindeki ABAP kodlarıyla otomatize edilir.
* **Kusursuz Veri Entegrasyonu:** ABAP aslında başından beri kendi içine "Gömülü Tipi/Veritabanı Dilidir (Open SQL)". Direkt olarak C# veya Java'daki gibi veritabanına bağlanma kütüphanesi kurmaz, SAP'nin devasa HANA veritabanına SQL gibi doğrudan saniyesinde dili kodlarken hükmeder.

## Dilin Mantığı ve Kod Yapısı
Çok çok eski nesil kaynaklı olan (COBOL diline fazlasıyla özenmiş) "İngilizce cümle yazıyormuş gibi" tasarlanmış dillerdendir. Bol miktarda harf ve cümle kelimesi vardır. Noktalı virgüllerle (`;`) değil, İngilizce düz cümlelerin bitişi gibi **Nokta (`.`)** ile biter.

Eskiden (90'lar) tamamen prosedüreldi ancak sonradan SAP bu dile Nesne Yönelimli (Object-Oriented ABAP - ABAP Objects) özelliklerini entegre ederek onu baya modernleştirdi. Modern ABAP'ta Java'ya benzeyen Class ve Method pratikleri hakimdir ancak görünüş itibariyle hala COBOL/Basic kırması, büyük harfli, aşırı spesifik (sadece iş verisi çözmek için tasarlanmış) bir canavar görünümüne sahiptir.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin ekrana veri basma komutu `WRITE:` şeklindedir. Bir string tanımak `DATA:` ile başlar. Java'daki gibi `{}` parantezlere hapsolmak yerine `LOOP AT` diyerek döngü başlar, `ENDLOOP.` noktası konarak döngü kapatılır.

### Örnek Bir ABAP Kodu: Veritabanı ve Şirket Raporu
Bir şirketin Fatura/Sipariş Kalemlerini (VBAK tablosu denir SAP'de) çekip bunu kullanıcının ekranına veya Excel tablosuna temizce basan klasik bir ABAP programı şeması:

```abap
* ABAP dilinde yildiz (Asterisk) satir basiysa yorum veya tırnak (") yorum satiridir.
REPORT z_fatura_raporu. " Programin adi 'REPORT' ile baslar. Nokta ile biter.

" 1. ADIM: TABLO VE DEGISKEN (DATA) TANIMLAMA (Tiptir)
" SAP arka plandaki VBAK (Satis Belgeleri) tablolarindan bir ic tablo (internal_table) kopyasi yapar.
DATA: it_faturalar TYPE TABLE OF vbak,  " Veritabanindan Cekilen Veriyi Tutacak Bellek Tablosu
      wa_fatura    TYPE vbak.           " Döngü için kullanılacak bir Okuma Satırı (Work Area)

" 2. ADIM: VERITABANINA DOGRUDAN SORGGU (Open SQL Entegrasyonu)
" Java veya C#'ın aksine, ABAP içinden direk SQL atarsınız, kütüphane gerekmez.
" VBELN(Fatura No), ERDAT(Tarih), NETWR(Tutar) kolonlarını SAP DB'den SELECT ile çeker.
SELECT vbeln erdat netwr
  INTO CORRESPONDING FIELDS OF TABLE it_faturalar
  FROM vbak
  WHERE netwr > 50000.  " Sadece Siparis Tutari (NETWR) 50 bin'den yuksekleri al

" 3. ADIM: RAPORLAMA VE DONGU (LOOP)
" Cektigi verileri donguye (LOOP) sokup ekrana/kağıda firlatır.
IF sy-subrc = 0. " sy-subrc, 'Son komut basarili mi?' diyen SAP Sistem Degiskenidir (0 ise Hata Yok).
  
  " Dongu baslangici: Tablodaki her bir faturayi wa_fatura'ya kopyala oku
  LOOP AT it_faturalar INTO wa_fatura.
    
    " Ekrana Formatsal Olarak Bas: (Tarih, Siparis Numarasi, Fiyat)
    WRITE: / 'Siparis No:', wa_fatura-vbeln,
             '| Tarih:', wa_fatura-erdat,
             '| Toplam Fiyat:', wa_fatura-netwr CURRENCY 'TRY'.
             
  ENDLOOP. " Donguyu Kapat
  
ELSE.
  " Eger sonuc yoksa hata mesaji (Message) kullaniciya popup oarak patlatilir
  MESSAGE 'Bu tutarin uzerinde kimse siparis vermemis!' TYPE 'I'.
  
ENDIF.
```
Bugün dev şirketlerin fabrikalarından çıkan ve tarafınıza yollanan faturaların o siyah-beyaz loglarında işte bu nokta (`.`) harfli büyük komutlar dizisi koşturur.

## Kimler Kullanır?
* Kariyer hayatı boyunca spesifik olarak **"SAP ABAP Uzmanı / Danışmanı"** unvanına sahip Enterprise Developer'lar. Kodları şirketlerin karanlık arka (Backend/ERP) tarafında kaldığı için sıradan Web/Mobil yazılımcılarla asla karşılaşmazlar.
* Endüstri Mühendisliğinden veya Bilgisayar Mühendisliğinden çıkıp büyük fabrikaların, dev E-Ticaret ve İnsan Kaynakları entegratör şirketlerinin otomasyon şablonlarını tasarlayan kodlayıcılar.
* (Son yıllarda ABAP; SAP'nin "Fiori ve OData" teknolojileriyle Web'e ve Javascript dünyasına da kapılarını mecburen sonuna kadar açmıştır).
