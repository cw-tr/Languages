# DAX (Data Analysis Expressions)

## Özet
DAX; 2010 yılında Microsoft tarafından icat edilen, Excel'in Klasik o basit formulasyonlarını (A1 + B2) Yeterli bulmayarak Onları patlatıp; Milyonlarca Satır veriyi İlişkisel Olarak (Table İlişkileri) harmanlayıp anında devasa zeka filtrelemeleri (Business Intelligence) Hesaplamaya sokabildiğiniz; Başta **Microsoft Power BI** ve Excel Power Pivot'un Kalbi olan, Fonksiyonel Analitik Formül ve Sorgu Dilidir.

## Nedir ve Ne İşe Yarar?
1990'lardan Beri muhasebeciler Veya Satışçılar EXCEL programında işlerini Görüyordu. 
Fakat Sizin Şirketiniz Büyüyünce; 1. Tabloda "5 Milyon Müşteri Satırı", 2. Tabloda "8 Milyon İade Satırı", 3. Tabloda "Tarih ve Dolar Kuru" tablosu (Excel yaprakları) birikince EXCEL ÇÖKÜYORDU. VLookUP (Düşeyara) Fonksiyonunu Yazınca Bilgisayar Saatlerce Kitlenir!

Microsoft Dedi Ki: "Formülleri Tek Bir Hücreye (Hücre Bazlı - Cell-based) yazmaktan Vazgeçeceğiz! Formülleri SÜTUN BAZLI (Sütun Tabanlı - Tüm Sütunlara Aynı Anda Hükmedilecek Şekilde) Kodlayacağız!" Ve Yarattığı O dilin Adı DAX idi.

**Ne İşe Yarar?**
* **İş Zekası (Power BI Dashboardları):** Koca Bir Şirketin "Geçen Yıl Aynı Ay Satışlarıyla, Bu Yılın Hafta sonu Satışlarının Karşılaştırılması" (Time Intelligence) Gibi Korkunç Matematiksel Dashboardlara (Görsel Grafiklere) Vurmak İçin; SQL Çok Uzun Sürer... DAX, O Grafiklerin "Measures (Ölçü)" Olarak Işık Hızında Kalbinde Atmasını Sağlar.
* Dinamik Filtreleme Zekası (Calculate Context'i): Patronunuz Grafikte FareYLE "Sadece İzmir Satışları" Nı Tıklattığı Anda; DAX Motoru Arka planda Bütün O "Toplam Ciro" Formülünü İptal eder, Veriyi Saniyesinde İzmir'e Göre Süzüp "Sihirli Bağlamda (Filter Context)" Yeniden Hesaplar!

## Dilin Mantığı ve Kod Yapısı
Görüntüsü, Excel Formüllerine Çok Benzer. (`=TOPLA() Gibi`). Ama Mantığı Excel'den %100 Farklıdır.
Excel'de Formül **Bir Hücreye (Örn Sütun İçindeki C2 HÜCRESİNE)** aittir.
DAX'ta Formül Bütün Tabloya Ya Da **Tüm Sütuna/Matematiksel Kavrama (Measure)** Aittir.

Sihirli Fonksiyonu `CALCULATE()` dir. Bütün Yazılan Kuralları (Filtreleri) Yıkarak Şartlı Yeni Toplamalar Yaratır. (Adeta C++'taki Pointerların Bükülmesi Gibidir).

### Örnek Bir DAX Kodu: Geçen Yılın Aynı Dönemine (Time Intelligence) Göre Ciro Büyüme Oranını Milisaniyede Hesaplayan Zeka!
Power BI İçerisinde, Şirket Patronuna Ekranda Kırmızı Veya Yeşil Ok Göstermek için Yazılan Muazzam "Ölçü (Measure)" Formül Çiftlikleri:

```dax
/* BU BIR MS DAX (Power BI Formul/Analitik) KODUDUR */

// 1. BASİT BİR ÖLÇÜ (MEASURE) YARATMAK = TÜM SATIŞLARIN TOPLAMI (CİRO)
// (Dikkat, Excel'deki Topla(A1:A500) gibi Hücre İsmi vermiyoruz. "Sales" Tablosunun "Amount" Sütununu Komple Topla diyoruz!)
Toplam_Ciro = SUM(Sales[Amount])


// 2. DAX'IN SİHRİ (CALCULATE) => GEÇEN YILIN AYNI DÖNEMİNİN CİROSUNU BUL!
// Patron Sağ Taraftan "Mayıs 2024" seçtiyse, Bu DAX kodu arkaplanda Onu İPTAL Edip "MAYIS 2023"'ün Toplamını Bularak Ekrana (Zamana Karşı) vurur!
Gecen_Yil_Ciro = 
    CALCULATE(
        [Toplam_Ciro],                         -- Neyi Hesaplayacağımızı (Ölçümü) Ver
        SAMEPERIODLASTYEAR('Date'[Date])       -- Ne Zamana Göre ? (DAX İçindeki Sihirli "Aynı Dönemin Geçen Yılı!" Kütüphanesi ZAMANI Vukuu ettirir!!)
    )


// 3. YÜZDELİK BÜYÜMEYİ HESAPLA VE PATRONA (GRAFİĞE) ÇAK !!
Ciro_Buyume_Orani_Yuzde = 
    // Eğer Adam Yeni Şirket Açmışsa Ve Geçen Yıl Sıfırsa (Division by zero ERROR Çökmesin Diye DIVIDE fonksiyonu Safety yaparak (Virgüllü Böler))
    DIVIDE(
        [Toplam_Ciro] - [Gecen_Yil_Ciro],   -- KAR / ZARAR Farkını BÖL!!
        [Gecen_Yil_Ciro],                   -- Geçen Yıla Böl (Yüzdeyi bul)
        0                                   -- Eğer Geçen Yıl Yoksa Hata Verme 0 (Boşluk) Çek !!
    )

```
Eğer İş Zekası (Power BI) Arayüzüne Bu Formülleri "Measure" Olarak Koyarsanız; Müşteri (SATIŞ Müdürü) O Arayüzde Çubuklu Grafiğe Tıklayıp "Bana Marmara'yı Göster" Dediğinde;
CALCULATE() Sihri Devreye girer ve O "Marmara" Filtresini TÜM Formüller İçinden Geçirerek Milyonlarca Satırlık BÜYÜMEYİ O an Saniyesinde Dinamik Olarak Çizdirir! Excel'in YAPAMAYACAĞI Mucize budur.

## Kimler Kullanır?
* Finansal Raporlamalar, Karlılık Analizleri Ve Şirketlerin Damarlarındaki Verileri "Görsel Sanata (Dashboardlara)" Döken Yükselen **Veri Analistleri (Data Analysts), İş Zekası (BI) Geliştiricileri ve Finans Kontrolörleri**.
* Microsoft'un Ekosistemindeki Milyar Dolarlık Power BI Piyassasını Yüzyılın En Güçlü Raporlama Aracı Yapan "Asıl Kası" Ve "Motoru" Dur. Sadece C++ yada Java Değil, Firmanın Kazancını "Bu Matematiksel Formüller" Hesaplar!
