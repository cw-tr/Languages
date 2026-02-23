# Apache Parquet

## Özet
Parquet (Parke); 2013 yılında Twitter ve Cloudera mühendisleri tarafından icat edilen, Apache Yazılım Vakfı destekli, Petabaytlarca (Trilyonlarca satır) büyüklüğündeki Devasa Verisetlerini (Big Data) Satır-Satır (Row-based) Excel gibi saklamak yerine; **"Sütun-Odaklı (Columnar)"** olarak SIKIŞTIRIP diske yazan, bu sayede hem Hard-Disk'inizdeki Gbyte masrafını %80 azaltan, hem de Okuma/Sorgu (Data Mining) Hızını JSON veya CSV'ye oranla **100 Kat Hızlandıran** dünyanın 1 numaralı Büyük Veri Saklama (Data Storage) Formatı ve Dilidir.

## Nedir ve Ne İşe Yarar?
Elinizde 1 Milyar insanın yaşadığı Şehirler, İsimler ve Yaşlar listesi olan bir CSV (Virgülle Ayrılmış Değer) Dosyası var. Bu dosya yaklaşık 500 GigaByte! (Sıradan JSON olsa 1 Terabayt tutar).
Veri Bilimcisi gelip Python'a Şunu yazar: **"Bana SADECE Yaşı 25 olan İnsanların SAYISINI Ver!"**
Eğer O 500 Gblık Dosya CSV veya JSON olursa; Makine 1.Satırdan (İsim, Şehir, Yaş) Başlar. Sonra 2. Satırı Okur (İsim Şehir, Yaş)... **Halbuki İsim ve Şehir umurumuzda Değil!** Boşu Boşuna o 500 GB Diske Sürürtüp Okunduğu İçin Cevabın gelmesi 40 Dakika Sürer!

Parquet Dediki: **Kardeşim, Verileri İnsan Okur Gibi Yan Yana (Satır Satır) Değil, AŞAĞIYA DOĞRU (SÜTUN SÜTUN) Parçalayarak Diske Kaydedelim!**
İsimlerin hepsi Bir Yerde "Zip"lensin. Şehirler Bir Yerde Sıkıştırılsın! Yaş Sütunu (18, 20, 25, 25, 25) Yan Yana Sıkıştırılsın! (Madem hepsi Rakam, O rakamlar birbirinin aynısı olacağı için Çok deli SIKIŞIR - Snappy Compression).

Sonuç? Veribilimcisi "Yaşları Oku" deyince Parquet, Dosyadaki İsimler ve Şehirler Bloğunu (Chunk) HİÇ UYANDIRMAZ (Disk IO = 0). Sadece Küçücük "Yaş" bloğunu 1 Saniyede RAM'e Çeker. 500 Gb CSV'nin Yaptığı İŞi **10 GB Parquet Formatı, 2 Saniyede Çözer!**

**Ne İşe Yarar?**
* **Big Data (Apache Spark / Hadoop):** Eğer Dünya Çapında Bir Firmaysanız (Trendyol, Netflix, Facebook), Veri Havuzunuza (Data Lake) Akan Satışların / Tıklamaların Kaydını Amazon Bulklarına Excel veya JSON Olarak Atmazsınız (Fakirleştirirsiniz). Milyarları PARQUET formunda Çevirip Saklarsınız; Bulut Faturanız Milyon Dolarlardan Bin Dolara Düşer.
* Kendi Kendini Anlatan Şema (Self-Describing): CSV dosyalarında Sütunun Tipi belli Değildir. Parquet'nin içine Şeması (Metadatası) Da Kodlanır "Bu sütun INT, Şu String" Gibi Güvenlidir.

## Dilin Mantığı ve Kod Yapısı
Parquet Sizin Oturup Elinizle (Notepad'den) Okuyabileceğiniz Bir "Metin Dili" Değildir. O İkili (Binary) Ve devasa Sıkıştırılmış bir Makine Formatıdır.
Onu Üretmek İçin (Python, Scala, Java veya SQL) Dillerini Kullanarak "Bu DataFrame'i PARQUET OLARAK DİSKE ÇIKART!" Diye Emredersiniz.

### Örnek Bir Parquet Mimarisi: (Python Pandas İle Devasa Hızda Sıkıştırma Kaydı)
Aşağıda Bir Veri Bilimcisinin İğrenç Derecede Yavaş Çalışan O CSV veri Setini (Kanseri), Evrenin En Hızlı Çekirdeklerinden Biri Olan Parquet Kod Formatına Püskürttüğü Çeviri Operasyonu:

```python
# BU BIR PYTHON PANDAS / PYARROW KODUDUR 
# (Parquet Dosyasi Okumammizi ve Yaratmmzii Saglar)

import pandas as pd

# 1. ESKI VE YAVAS DUNYADAN (CSV) DATAYI OKU (Mesela 10 Milyon Satirlik Veri!)
yavas_veri = pd.read_csv("musetri_harcamalari.csv") 


# 2. DEVASA YENİLİK: CSV'Yİ AL VE ONU DİSKE "PARQUET" (.parquet) OLARAK FIRLAT!!
# Parquet Motoru (engine='pyarrow') Kullanarak Sütunları Parçalayıp Matematiksel Zipe Sokacaklar!
yavas_veri.to_parquet(
    "isik_hizinda_musteriler.parquet", 
    engine='pyarrow', 
    compression='snappy' # Google'ın Hızlı Zippleyicisi Snappy ile Sıkıştır (GigaBytelar MegaByta Düşsün)
)


# -------------------------------------------------------------
# 3. YARIN GELİP AYNI DOSYAYI OKUDUĞUNDA ALACAĞIN İHTİŞAM (PERFORMANS)!
# Veri Bilimcisi: "Bana O Milyonlarca Adamdan SADECE 'ulkesi_id' si 5 Olanların Maaşını Çek!"

# Parquet SİHRİ: COLUMNS KAVRAMI (Sadece İlgili Sütunları Oku!! Gerisini Elleme!!!!)
hizli_cevap = pd.read_parquet(
    "isik_hizinda_musteriler.parquet",
    columns=['il_id', 'aylik_harcama']  # Parquet Olarak Yazıldığı İçin Dosyanın Tamamını TARAMAZ!!.
)                                       # Sadece O diskteki 2 Sütuna Nokta Atışı Uçar Ve Saniyede Döner!

```
Görüldüğü gibi Parquet "Aptal bir String (Metin)" Dosyası Olmaktan Ziyade, Disk Üzerinde Çalışan Bir **Veritabanı Motoru Gibi** Davranır (Hangi sütunun Nerede başladığını, Nerde bittiğini ve Maksimum Minimum değerlerini İçinde Metadata olarak Tutar).

## Kimler Kullanır?
* Evrendeki bütün (100 Mb'dan büyük verilerle uğraşan) **Veri Mühendisleri (Data Engineers), Big Data (Hadoop/Spark) Mimarları ve Apache İşçileri**.
* Büyük şirketlerde (Günde Milyonlarca Log Akan Sunucularda) Depolama maliyetlerinden (AWS S3) Yüzde 80 Tasarruf Ettirdiği Ve BI (Business Intelligence) Araçlarına Süper Hız Sağladığı İçin JSON'un Tahtını Veri Ambarında (Data Warehouse) Gözünü kırpmadan Yıkan Yüce Bir Standarttır. İstisnasız her veri bilimcisi hayatını Parquet ile mühürleyecektir!
