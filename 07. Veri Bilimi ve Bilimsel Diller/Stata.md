# Stata

## Özet
Stata; 1985 yılında StataCorp kurucusu William Gould tarafından yaratılan, tamamen **Ekonometri**, Tıbbi Epidemiyoloji (Hastalık yayılması) ve Sosyal Politik Bilimlerdeki devasa istatistiksel veri analizlerini, "Zarif tek-satırlık ezberlenebilir emir komutlarıyla (Command-Line Driven)" milisaniyeler içinde yapan muazzam sektörel İstatistik Analiz dili ve aracıdır.

## Nedir ve Ne İşe Yarar?
1980'lerde ve 90'larda SPSS fare (GUI) ile menüden araştırma yapıyor, SAS devasa 10-Sayfalık "Data/Proc" sunucu kodlarıyla çalışıyordu. Siyaset Bilimciler ise (örn: Seçim etkileri istatistikleri, Hastalık ilaç ölüm oranları hesaplayan Epidemiyologlar) "Biz fare(Bunu amatörler yapar) tıklamak istemiyoruz, aşırı uzun SAS kodlarını makaslamak da istemiyoruz! Bize Terminalden/Komuttan anında emir verebileceğimiz; `regress (Regresyon Al) Muaas Egitim Cinsiyet` yazdığımızda şak diye tablosunu veren bir kod lazım" diyordu.

Stata tam olarak bu eksiği bir komut kılıcı gibi tamamladı. Hem Menüsü vardır (Fareyle tıklanabilir) lakin Stata'nın gerçek gücü **Stata "Do-File" (.do)** (Yap Dosyası) dediğimiz devasa Komut betik dilleridir. Stata, bir makro-ekonomistin tek saniyede grafiği bulup ekrana sabitlediği niş/kült veri bilimi makinesidir.

**Ne İşe Yarar?**
* **Panel Veri Analizi (Ekonometri):** Merkez Bankaları analistleri, "10 Yıl boyunca 50 farklı Ülkenin Enflasyon ve Büyüme rakamlarını" yan yana dizebilmek için (Panel Data) Python/Pandas yerine saniyede `xtreg` komutlarıyla Stata kullanırlar.
* **Tıp ve Halk Sağlığı (Epidemiyoloji):** İlaç veya "Covid Hastalarında aşının Aylar içinde hayatta kalma (Survival Analysis)" grafiklerini çıkaran Orijinal Kaplan-Meier tabloları en yoğun şekilde Stata (`sts graph`) koduyla akademide tıpçılar tarafından çizdirilir. Yüksek hızlı ve pürüzsüzdür.

## Dilin Mantığı ve Kod Yapısı
Tam bir **Pragmatist Emir (Command-Driven)** dilidir. Hafızasında DAİMA tek bir büyük Aktif (Mevcut) Tablo / Dataset bulunur (R'daki gibi aynı anda 50 Tablomu (Dataframe) olsun diyemezsiniz (Eskiden böyleydi modern stata bunu biraz kırdı)). Programcı veriyi çeker (`use`), komutları sıralar ve kapatır. Tüm komutların kısaltılmış bir harf hali vardır (Regression için sadece `reg` yazabilirsiniz).

Her komut genellikle `emir hedef_değişkenler, secenekler` şablonuna uyar. En büyük felsefesi: **Tüm işlemler virgülle ( , ) ikiye bölünür**. Virgülün Solu Emiri ve sütunları(Örn y ve x kordinatları), Virgülün SAĞI ise (Ayrıntıları, Opsiyonları: Standart hata yapsın mı? Grafiğin rengi ne olsun?) işaret eder!

**Örnek İşleyiş (Sembolik Olarak):**
Python'da Regresyon: `model = sm.OLS(y, X); sonuc = model.fit(); print(sonuc.summary())` (Amelelige bakin)
Stata'da: `regress Maasi Egitimi Yas_Yili` (Bitti!)

### Örnek Bir Stata (Do-File) Kodu: Ekonometrik Maaş-Eğitim "Linear Regresyon" Formülünü Sökmek
Makro İktisat mezunu bir Ekonometristin, Pythoncuları 10 saniyede dumura uğratıp Milyonlarca Datayı Çizip/Regresyon aldığı o akıcı kısaltılmış Emir Dünyası:

```stata
/* Stata (Do-File) formatinda yorumlar C tarzidir veya Yildiz ile baslar */

// 1. VERİYİ HAFIZAYA YÜKLEME 
// Stata'nin hafizasini pürüzsüz temizle (clear)
clear all

// Internetten (veya yerel C diskinden) bir Veritablosu (dataset.dta) Çek ve Kullan (Use):
// Meşhur 'Otomobil Fiyatari/Agirliklari (auto.dta)' egitim setini hafizaya Cek:
sysuse auto, clear


// 2. VERİYİ TANIMA VE TEMİZLİK KESİNTİLERİ
// Data'da ne var? Ozet (Summary = sum) cikar (Ekrana aninda Ortalama/Standart sapma duser)
summarize price mpg weight

// YENI BIR SÜTUN(DEĞİŞKEN) YARAT (Generate komutu veya kisaltmasiyla 'gen' !)
// Arabanin Agirligininkaresini Yeni Stun yap:
gen weight_sq = weight^2

// Filitreleme: Mil/Galon Miktarı (mpg) 30'dan kucuk olan Arabalari SİSTMDEN TAMAMEN SIL(Drop):
drop if mpg < 30


// 3. İSTATİSTİKSEL MODELLEME (ÇEKİRDEK GÜÇ) VE VİRGÜL MİMARİSİ
// Araba fiyatinin (Price = Y(Eksem)),  Araba agirligi(weight) ve Yabanci olup olmamasinin(foreign) (X Eksenleri) tarafindan nasil Etkilendiğini/Arttigini Olc! Coklu Linear Regression:

regress price weight foreign, robust

// DİKKAT: Virgül ( , ) isareti Stata'nin sihirlisidir! Virgulun sagi Opsiyonlar demektir. 
// "robust" -> "Bana Heteroskedastik(Varyansi Oynak) verileri Engelle, Güclü(Robust) Sapma Hatasi ver" komutudur.


// 4. KAPANIŞ: GÖRSELLESTİRME (GRAFİK ÜRETIMI)
// Scatter (Nokta Grafik Ciz: Y=Price, X=Weight)
// İcine bir tane Dogrusal(lfit - Linear Fit OLS cigzizi) firlat, Renkgi/Efsanelesini virgulle ayarla:

twoway (scatter price weight) (lfit price weight), ///
    title("Araba Fiyatları vs Agirlik OLS Çizgisi") ///  
    xtitle("Agirlik Lbs") ytitle("Fiyat Amerikan Dolari") /// 
    legend(off)

// NOT: '///' (Uc taksim) Stata'da komutun bi sonraki satira sarktigini/Birlestigini ifade eder!
```
Siz bunu IDE'de Seçip `Ctrl+D` (Do - Çalıştır) dediğiniz an saniyeler içinde bütün istatistik P-Valueleri (0.05 ehemmiyeti) ve rengarenk grafik analizleri bir makale kağıdına yazılır gibi Stata ekranına (Results window) dökülür.

## Kimler Kullanır?
* Klasik İktisat Uzmanları, Makro Ekonomi ve Finans(Ekonometri) Hocaları, Ekonomi Bakanlığı Müsteşar ve Veri Analistleri. 
* Dünya Sağlık Örgütü (WHO) gibi yerlerde Kistik/Genetik veya Pandemik Hastalık Büyüme Hızını istatistiğe vuran Epidemiyologlar (Panel Data kütüphanesi ve Survival kütüphanesi evrendeki en iyi sistemlerden biridir).
* Maliyeti SAS kadar delice olmadığı için Avrupa üniversitelerinde Ekonomide ana dal programı olarak C#/Python'dan bile önce eğitimde kodlatılır.
