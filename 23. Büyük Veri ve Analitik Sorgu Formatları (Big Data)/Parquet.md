# Apache Parquet

## Özet
Parquet (Parke); 2013 yılında Twitter ve Cloudera mühendisleri tarafından icat edilen, Apache Yazılım Vakfı destekli, Petabaytlarca (Trilyonlarca satır) büyüklüğündeki Devasa Verisetlerini (Big Data) Satır-Satır (Row-based) Excel gibi saklamak yerine; **"Sütun-Odaklı (Columnar)"** olarak SIKIŞTIRIP diske yazan, bu sayede hem Hard-Disk'inizdeki Gbyte masrafını %80 azaltan, hem de Okuma/Sorgu (Data Mining) Hızını JSON veya CSV'ye oranla **100 Kat Hızlandıran** dünyanın 1 numaralı Büyük Veri Saklama (Data Storage) Formatı ve Dilidir.

## Nedir ve Ne İşe Yarar?
Elinizde 1 Milyar insanın yaşadığı Şehirler, İsimler ve Yaşlar listesi olan bir CSV (Virgülle Ayrılmış Değer) Dosyası var. Bu dosya yaklaşık 500 GigaByte! (Sıradan JSON olsa 1 Terabayt tutar).
Veri Bilimcisi gelip Python'a Şunu yazar: **"Bana SADECE Yaşı 25 olan İnsanların SAYISINI Ver!"**
Eğer O 500 Gblık Dosya CSV veya JSON olursa; Makine 1.Satırdan (İsim, Şehir, Yaş) Başlar. Sonra 2. Satırı Okur (İsim Şehir, Yaş)... **Halbuki İsim ve Şehir umurumuzda Değil!** Boşu Boşuna o 500 GB Diske Sürürtüp Okunduğu İçin Cevabın gelmesi 40 Dakika Sürer!

Parquet Dediki: **Kardeşim, Verileri İnsan Okur Gibi Yan Yana (Satır Satır) Değil, AŞAĞIYA DOĞRU (SÜTUN SÜTUN) Parçalayarak Diske Kaydedelim!**
İsimlerin hepsi Bir Yerde "Zip"lensin. Şehirler Bir Yerde Sıkıştırılsın! Yaş Sütunu (18, 20, 25, 25, 25) Yan Yana Sıkıştırılsın! (Madem hepsi Rakam, O rakamlar birbirriin aynısı olcucağı için Çok deli SIKISIR - Snappy Compression).

Sonuç? Veribilimcisi "Yaşları Oku" deyince Parquet, Dosyadaki İsimlerve ve Şehirler Blopunu (Chunk) HİÇ UYANDIRMAZ (Disk IO = 0). Sadece Küçücük "Yaş" blogunu 1 Saniyede Rama Çeker. 500 Gb CSV'nin Yaptığı İŞi **10 GB Parquet Formatı, 2 Saniyede Çözer!**

**Ne İşe Yarar?**
* **Big Data (Apache Spark / Hadoop):** Eğer Dünya Çapında Bir Firmaysanız(Trendyol, Netflix, Facebook), Veri Havuzunuza (Data Lake) Akan Satışların / Tıklamlaarın Kaydını Amazon Bulklarına Excel veya JSON Olarak  Atmazsınız(Fakirleştirirsiniz). Milyarları PARQUET formunda Çevrip Saklarsınız; Bulut Faturanız Milyon Dolarladan Bin Dolara Düşer.
* Kendi Kendini Anlatan Şema (Self-Describing): CSV dosyrlarında Sütunun Tipi belli Değlmdir. Parquet'nin içine ŞemasI(Metadatasi) Da Kodlanır "Bu suütan INT. Susa String" Gibi Güvenlidir.

## Dilin Mantığı ve Kod Yapısı
Parquet Sizn Oturup Elinilze (Notpad'den) Okuyabileğeniz Bir "Metin Dili" Değildir. O İkili (Binary) Ve devasa Sıkıştırılmış bir Makine Formatırdır.
Onu Üretmek İçin (Python, Scala, Java veya SQL) Dillerini Kullaanrak "Bu Dataframa'i PARQUET OLARAK DİSKE CIKART!" Diye Emrredrinsiiz.

### Örnek Bir Parquet Mimarisi: (Python Pandas İle Devolası Hızda Sıkıştırma Kaydı)
Aşağıda Bir Veri Bilimcisinin İğrenç Derecede Yavaş Çalışan O CSV veri Setini (Kanseri), Evrenin En Hızlı Çekireklerinden Biri Olan Parquet Kod Formatına PÜsküttüğü Çeviiri OPerasyonu:

```python
# BU BIR PYTHON PANDAS / PYARROW KODUDUR 
# (Parquet Dosyasi Okumammizi ve Yaratmmzii Saglar)

import pandas as pd

# 1. ESKI VE YAVAS DUNYADAN (CSV) DATAYI OKU (Mesela 10 Milyon Satirlik Veri!)
yavas_veri = pd.read_csv("musetri_harcamalari.csv") 


# 2. DEVASA YENILIK: CSV'YI AL VE ONUD DİSKE "PARQUET" (.parquet) OLAAK FİRLAT!!
# Parquet Motoru (engine='pyarrow') Kullanarak Sutunlari Parcalayipp Matematiksel zipe Sorarcaklar!
yavas_veri.to_parquet(
    "isik_hizinda_musteriler.parquet", 
    engine='pyarrow', 
    compression='snappy' # Google'in Isci Ziplayicisi Snappy ile Skistr (GigaBytelar MegaByta Dussunn)
)


# -------------------------------------------------------------
# 3. YARİN GELİP AYNİ DOSYAYİ OKUDGUNDA ALACGIN IHTSIAM(PERFORMANS)!
# Veri Bilmcisi: "Bana O Milyoanarca Adamdan SADECE 'ulkesi_id' si 5 Olanlarin Maasii Cek!"

# Parquet SİHRİ: COLUMNS KAVSRAMI(Sudece İlgilei Surunltari Oku!! Gerisini Elleem!!!!)
hizli_cevap = pd.read_parquet(
    "isik_hizinda_musteriler.parquet",
    columns=['il_id', 'aylik_harcama']  # Parquet Olarak Yaziidigi İçin Dosyanin Tamamini TARAMAZ!!.
)                                       # Sadece O diskedeki 2 Sütüna Nokta Atişi Ucar Ve Sanaiuyede doner!

```
Görüldüğü gibi Parquet "Aptal bir String (Metin)" Dosyası Olmaktan Ziyade, Disk Üzerinde Çalışan Bir **Veritabanı Motoru Gibi** Davaranır (Hangi sütunun Nerede başladını, Nerde bittignii ve Maksiumam Minumum değerleirni İçinde Metadaa olarak Tutar).

## Kimler Kullanır?
* Evrendeki bütün (100 Mb'dan büyük verilerler uğarşan) **Veri Mühendisleri (Data Engineers), Big Data (Hadoop/Spark) Mimarları ve Apache İşçileri**.
* Büyük şirketlerde (Günde Milyonlaraca Log Akan Sunucularda) Depolama maliyetlerinden (AWS S3) Yüzde 80 Tasarruf Ettirdiği Ve BI (Business Intelligneee) Araçlarına Süper Hız Sağladıpıçın JSON'un Tahtını Veri Ambarında(DataWarehosue) Gözünü kırpadan Yıkan Yüce Bir Standattr. İstisasıx Her veri bilimcisi Hayatına Parquet İle Mühürliyecektiri!
