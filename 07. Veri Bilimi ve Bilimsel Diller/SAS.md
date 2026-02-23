# SAS

## Özet
SAS (Statistical Analysis System); 1976 yılında Kuzey Karolina Eyalet Üniversitesi'nde yaratılan ve daha sonra SAS Institute adlı devasa şirket tarafından ticarileştirilen, istatistiksel analiz, veri madenciliği ve tahmine dayalı modelleme (Predictive Analytics) alanında bugün hala **Dünyanın En Büyük İlaç ve Banka Şirketlerinin 1 Numaralı Ticari Veri Dilidir**.

## Nedir ve Ne İşe Yarar?
1970'lerde veri bilimi (Data Science) diye bir kavram yoktu, Python ve R çok uzak bir hayaldi. Hastanelerde binlerce hastadan gelen klinik deney sonuçları (örn: Kanser ilacı etki verileri) veya tarım arazilerinin verim rakamları delikli kartlarla IBM Mainframe bilgisayarlarına giriliyordu. 

İşte SAS, bu devasa verileri içeri çekebilen, formüllerde temizleyebilen ve istatistik raporu olarak yazıcıdan fırlatan bir sistem/dil paketi olarak doğdu. Açık kaynak değildir (Aşırı pahalı lisansları vardır). Ancak bir dili bu kadar pahalı yapmasına rağmen endüstri standardı tutan şey şudur: "Eğer bir Kanser İlacını Piyasaya sürmek için Amerika Sağlık Bakanlığına (FDA) başvuracaksanız, verinin %100 oranında güvenilir olan **SAS Sistemi formül çıktılarıyla ve SAS Kodlarıyla kanıtlanmış olması yasal bir zorunluluk/gelenekti**".

**Ne İşe Yarar?**
* **Klinik Araştırmalar Ve İlaç Sanayisi:** Biyolojik ve klinik test sonuçlarının (Pharma endüstrisi) tüm test aşama (Phase I-IV) geçerlilik hesapları dünyadaki tüm tıbbi firmalar tarafından SAS diliyle yapılır.
* **Kurumsal Bankacılık ve Kredi Skorlama:** Bankaların (Örn: JP Morgan, Garanti BBVA) müşteriye kredi verirken "Bu kişi kredisini geri öder mi?" istatistik tahmini (Risk Modellemesi), SAS'ın meşhur `PROC LOGISTIC` komutlarıyla hesaplanır. 
* **Büyük Data Manipülasyonu:** Python PANDAS kütüphanesi yokken, devasa Terabyte'lık tabloları hafıza çökmeden birleştiren (Merge) 40 yıllık en stabil canavardır.

## Dilin Mantığı ve Kod Yapısı
Tam bir "Adımlı/Prosedürel (Step-based)" yapıya sahiptir. Program iki temel dev bloktan (Step) oluşur:
1. **DATA Step (Veri Adımı):** Veriyi Excel'den veya veritabanından alıp yeni şartlara göre keser, temizler ve hafızaya dize yazar. İşlem satır basında (Satır satır/Record-based) otomatik döndürülür (For döngüsü gerekmez).
2. **PROC Step (Prosedür Adımı):** DATA alanındaki veriyi alıp hazır İstatisitik/Rapor fonksiyonlarıyla (Ekrana T-Test Cizmek, Korelasyon tablosu çizmek vb) anında görseleştirir ve hesaba patlatır. Bütün analitik PROC (Procedure) fonksiyonlarından gelir.

Cümleler (Statements) C dilindeki gibi muhakkak noktalı virgül (`;`) ile biter ve Bloklar `RUN;` emri ile tetiklenir ("Hadi artık bu bloğu çalıştır!").

**Örnek İşleyiş (Sembolik Olarak):**
Python'da Matris çağırmak: `import stat_kutuphanesi; stat_kutuphanesi.hesapla(dataframe)`
SAS'ta: `PROC MEANS data=benim_listem; RUN;` (Bitti. Size yüzlerce sayfa varyans, ortalama, t-test raporu HTML'i fırlatır.)

### Örnek Bir SAS Kodu: Klinik Hasta Verisini Okuyup İstatistik Çıkarmak
Aşırı derecede İngilizce konuşma ve SQL benzeri okuma yapısına sahip o meşhur DATA/PROC (Prosedür) basamak kuleleri:

```sas
/* SAS dilinde yorum satirlari C tarzidir (/* */) veya basinda Yildiz Sonunda Noktali Virgul (*) kullanilir */
* BU BİR SAS YORUM CÜMLESİDİR;

* ========================================= ;
* 1. DATA STEP (Veri Üretme veya Çekme Modülü)
* ========================================= ;

* 'Klinik_Hastalar' Isminde bir hafiza tablosu (Dataset) olusturmaya basla:;
DATA Klinik_Hastalar;
   
   * Sisteme verilerin TIPLERINI tanit: Hasta_ID (Sayi), Yas (Sayi), Cinsiyet (Karakter - $) ve Ilac_Gucu (Sayi);
   INPUT Hasta_ID Yas Cinsiyet $ Ilac_Gucu;
   
   * Yeni Bir KURAL/Stun Yarat!: "Eger Hasta 50 Yasindan buyukse onu YUKSEK RISK kategorisine Ata!";
   IF Yas > 50 THEN Risk_Grubu = 'YUKSEK';
   ELSE Risk_Grubu = 'NORMAL';
   
   * Sisteme el ile 5 satir veri firlatiyoruz (CARDS veya DATALINES komutuyla);
   DATALINES;
   101 45 E 12.5
   102 62 K 18.0
   103 31 K 9.2
   104 55 E 22.1
   105 48 K 14.5
   ; * Veri girisi bitti.
   
RUN; * SİHİRLİ KELİME Düğmeye Bas! SAS Motoru veriyi saniyede RAM'e isledi.


* ========================================= ;
* 2. PROC STEP (İstatistiki Analiz Modülü)
* ========================================= ;

* PROC MEANS: "Ortalama (Mean), Standart Sapma, Maks ve Min" bulan Meşhur İstatistik Prosedürü!;
* SAS'a Diyoruz ki: Data olarak 'Klinik_Hastalar' tablosunu kullan! Sadece "Ilac_Gucu" nün ortalamasini ver;
PROC MEANS DATA=Klinik_Hastalar MEAN STD MIN MAX;
   VAR Ilac_Gucu;
   TITLE 'Klinik Deney: İlaç Gücü İstatistiksel Özeti';
RUN;


* EKSTRA PROC: FREKANS RAPORU!
* Kim YUKSEK RISKLI kim NORMAL riskli? Proc Freq (Frequency) sayım raporu yapsın:;
PROC FREQ DATA=Klinik_Hastalar;
   TABLES Risk_Grubu * Cinsiyet; * Tablonun X ve Y kordinatina hangi Sütunlarin Sayisi Gelsin? ;
   TITLE 'Risk Gruplari ve Cinsiyet Dagilimi Matrisi';
RUN;
```

Eğer kodu Run derseniz, arka planda muazzam güçlü SAS Analytics Motoru (Java ve C ile yazılmıştır) çalışarak şirketinizin Browser (SAS Studio) ekranına renklendirilmiş şık raporlama formları basar.

## Kimler Kullanır?
* Açık kaynak dilleri R veya Python kodlamasını sevmeyen veya "Python kütüphaneleri sürekli güncellenip kırılıyor, bana 30 yıl çökmeden çalışacak lisanslı sigorta lazım" diyen **Uluslararası Dev Bankalar, Sigorta Şirketleri**.
* Tıp verilerinin güvenliğinde dünyadaki bütün **İlaç (Pharma) Araştırma Geliştirme** istatistik uzmanları. (Hastalık araştırmalarının %90'ı SAS raporlarından çıkar).
* Özel üniversitelerdeki İstatistik hocaları. Ancak yüksek fiyatlı lisanlara sahip olduğu için, küçük girişimciler(Start-Up) ve Veri Bilimi "Kaggle" yarışmacıları SAS yerine tamamen bedava/ve esnek olan `Python/R` ikilisini kullanır. Modern günümüzde "Python" hızla SAS'ın tahtını sallamaktadır.
