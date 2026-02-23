# SPSS

## Özet
SPSS (Statistical Package for the Social Sciences - Sosyal Bilimler İçin İstatistik Paketi); 1968 yılında Norman Nie ve meslektaşları tarafından (şimdilerde dev IBM bünyesinde) yaratılan, **Dünyadaki tüm Sosyoloji, Psikoloji, Sağlık ve Pazarlama akademisyenlerinin "Anket Verilerini" analiz ettiği**, formül/kod bilmeden "Tıklama (GUI)" hızıyla Sosyal Bilimler istatistiği yapmalarını sağlayan bir yazılım; fakat temelinde `SPSS Syntax` isminde efsanevi bir gizli arka-plan programlama (kodlama) sistemidir.

## Nedir ve Ne İşe Yarar?
1970'li yıllarda Üniversitelerde Anket (Survey) yapan Sosyolog ya da Siyaset Bilimciler; ellerindeki örneğin "Seçmen Davranışı" cevaplarını derlemek için Fortran kodlamayı öğrenmek zorundaydı. Fakat Sosyal bilimcilerin ne C kodlayacak ne de Matematik Matrixleri formüle edecek vakitleri/niyetleri yoktu.

IBM SPSS, ekranı sanki bir **Excel Tablosu** gibi çizdi (Data View). Bir psikolog hasta verilerini düz excel gibi elle kutulara yazıyordu, üst menüden `Analiz -> Bağımsız T-Testi` tuşuna farenin (Mouse) sol kliğiyle tıklıyordu ve SPSS bir saniye sonra tezin(Makalenin) grafiğini ve "P-Sayısını" ekrana çiziyordu. Ancak işin o Fare ile tıklanan ekranının arka planında (Log'da) o kliği koda dönüştüren şey: SPSS Syntax Programming Language dilidir.

**Ne İşe Yarar?**
* **Akademik Anket/Tez Analizleri (Likert Ölçeği):** Dünyadaki herhangi bir üniversitenin Yüksek Lisans veya Doktora öğrencisi, "insanlara" anket uyguladıysa ve "Öğrencilerin sınav kaygısı ile cinsiyetleri arasında fark var mı?" gibi sorular soruyorsa %95 ihtimalle tezindeki grafikler SPSS ile çıkarılmıştır. (Crombach Alpha güvenilirliği vb.)
* **Pazar Araştırması (Market Research):** Şirketlerin (örn: Tüketici Memnuniyeti raporları) "Ürünümüzü alanlar ev hanımı mı memur mu?" analizleri için en çok başvurulan veri istasyonudur.

## Dilin Mantığı ve Kod Yapısı
SPSS temelde bir "Uygulama" olarak kullanılsa da, ciddi veri madencileri o GUI (Arayüz) arayüzünü kullanmazlar; menü tıklama yerine direkt **SPSS Syntax Code (Kod Penceresinde)** işlem sürdürürler. Bu bir kod dilidir! 

Sentaks dili tamamen veri manipüle etmek ve İstatistik test çağırmak için İngilizce Kalıp Cümlelerden (PROC, COMPUTE) meydana gelir. Satırların sonu her zaman bir Nokta `.` işaretiyle biter. Kodlar büyük şablonlardan (`FREQUENCIES`, `CROSSTABS`) ibarettir çünkü döngüye gerek yoktur.

**Örnek İşleyiş (Sembolik Olarak):**
Eğer fareyle "Anket Raporu frekansını çıkart" tuşuna basarsanız (veya koda dökerseniz) SPSS arka planda şu gizli kodu işletir:
`FREQUENCIES VARIABLES=Cinsiyet Egitim.`

### Örnek Bir SPSS Syntax Kodu: Değişken Recoding ve Çapraz Tablolama (Cross-tab)
Menü kullanmadan devasa bir Siyasi/Psikolojik anketi hızla (Programlama felsefesiyle) temizleyen ve analizleyen klasik "Syntax (Sözdizimi) Dili" betiği:

```spss
* SPSS DILINDE Yorum komutlari yildiz ile isaretlenir ve mutlaka Nokta (.) ile bitmek zorundadir! .
* "BİR SEYI DEGISTIR (COMPUTE/RECODE) O HALDE ONU OYLE KAYDET (EXECUTE)" felsefesinin kalbi.

* 1. VERİ TEMİZLİĞİ: "COMPUTE" ILE YENİ BİR DEĞİŞKEN (Sütun) YARAT
* Anketörün girdiği "Boy" ve "Kilo" sutunlarindan, formülle BMI (Vücut Kitle Indeksi) Sutunu hesapla ve Dosyaya Ekle:
COMPUTE VucutKitleIndeksi = Kilo / ((Boy/100) * (Boy/100)) .
EXECUTE .


* 2. RECODE (YENİDEN KODLAMA) - Efsanevi Kategorik Kesici!
* Elimizdeki "Yas" sutunun daki (Sayisal verileri), Anket Gruplarina(1 Genc, 2 Yasli) cevir!
* Eger 18'den kucukse (Thru 18) ona 1 degeri ver. 19 ile 35 arasina 2 de. Sonra Bunu YENI BIR STUNA (YasGrubu) aktar.
RECODE Yas 
  (LOWEST THRU 18 = 1) 
  (19 THRU 35 = 2) 
  (36 THRU HIGHEST = 3) 
  INTO YasGrubu .
EXECUTE .

* Rakamlar (1,2,3) okurken hos durmaz (Sosyal Bilimciler rakam sevmez), onlari Metne cevir(Labeling):
VALUE LABELS YasGrubu
  1 '18 Yas Alti (Genc)'
  2 '19-35 Yas Arasi (Yetiskin)'
  3 '36 Ustu (Yasli)' .


* ========================================= .
* 3. ISTATISTIKSEL ANALİZ YAZILIM / RAPOR BÖLÜMÜ 
* ========================================= .

* FREKANS TABLOSU CIZ! Ankete kimler katilmis listele.
* "Istatistikleri ayrica Median(Ortanca) ve Mode olarak istiyorum. Ekrana Grafik(Bar chart) bastir!"
FREQUENCIES VARIABLES=YasGrubu Cinsiyet
  /STATISTICS=MEDIAN MODE
  /BARCHART .


* CROSSTABS (ÇAPRAZ TABLO VE KI-KARE TESTI!)
* Cinsiyet ile Secim_Oy_Davranisi arasinda "İstatistiksel olarak Anlamli fark/Ilıski var mi?" diye Test et (Kikare / Chisq yolla)
CROSSTABS
  /TABLES=Cinsiyet BY Oy_Verdigi_Parti
  /STATISTICS=CHISQ .
```

Eğer elinizde 50.000 kişilik ulusal bir seçim anketi varsa; menülerde tıklayıp yorulmak yerine bu SPSS Syntax kod bloğunu yapıştırıp `Run (Çalıştır)` derseniz, 3 saniyede bütün grupları temizler, verinin yaş/kilo gruplarını keser ve devasa şık tez tabloları fırlatır.

## Kimler Kullanır?
* Evrendeki Sosyal Bilimciler: PDR, Psikoloji Uzmanları, Pazarlama (Marketing) Direktörleri, Sosyoloji ve Siyaset Bilimcileri.
* Kod yazmaktan ölesiye korkan ama "İstatistik/Hipotez" ispatlamak zorunda olan Tesisat Doktora Hekimleri (Tıpta da anket kullanıldığında yaygındır).
* Veri Bilimi ve Yapay Zeka (AI) endüstrisinde yeri %0'dır. Yalnızca Klasik Hipotez Testi ve Sosyal çıkarım amaçlı kullanılır; o yüzden Bilgisayar Mühendislerinin radarında hiçbir zaman olmamıştır, tamamen **Sosyal Bilim Formatiğine** entegre olmuş bir tekeldir.
