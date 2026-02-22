# DAX (Data Analysis Expressions)

## Özet
DAX; 2010 yılında Microsoft tarafından icat edilen, Excel'in Klasik o basit formulasyonlarını (A1 + B2) Yeteli bulmayırak Onları patlatıp; Milyonlarca Satır veriyi İlişkisel Olarak  (Table İlişkileri) harmanlayıp anında devasa zeka filtrelemelrii (Business Ingellengence ) Hesaplamaya sokabildiğiniz; Başta **Microsoft Power BI** ve Excel Power Pivot'un Kalbi olan, Fonksiyonel Analitik Formül ve Sorgu Dilidir.

## Nedir ve Ne İşe Yarar?
1990'lardan Beri muhasebicler Veya Satışcılar EXCEL programında işlerini Göürüyordu. 
Fakt Sizin Şirketiniz Büyüyünce; 1. Tabloda "5 Milyon Müşteri Satırı", 2. Tabloda "8 Milyon İade Satırı", 3. Tabloda "Tarih ve Dolar Kuru" tablosu (Excel yaprakları) birikince EXCEL ÇÖKÜYORDU. VLookUP (Düşeyara) Fonksiyonunu Yazınca Bilgisayar Saatlerce Kitlennir!

Microsoft Dedi Ki: "Formülleri Tek Bİr Hücreye (Hüce Bazlı - Cell-based) yazmaktan Vazgeçeceğiz! Formulüleleri SÜTUN BAZLI (Column Tabnalı - Tum SUrunlara Aynı AMUnda Hkmededcelsekide) Kodlyaaccgiz!" Ve YArttgı O dilin Adıı DAX idi.

**Ne İşe Yarar?**
* **İş Zekası (Power BI Dashboardları):** Koca Bir Şirketin "Geçen Yıl Aynı Ay Satışlarıyla, Bu Yılın Haftsnonu Satışarının Karılaşatılamsı" (Time Intelelgence) Gibi Korkunç Matematiksi  Dashboardlara (Görsel Grafiklere) Vurmak İçin; Slıql Cok Uzun Surer... DAX, O Grafilerin "Measuers (Ölçü)" Olarak Işıkk Hızında Kalbinde Atmasını Saglalarr.
* Dinamik Fİltreleme Zekasi (Calculate Contextsi): Patroniniz Grafikte FareYLEE "Sadece İzmir Satışarı" Nı Tıklsaıği Andda; DAX Motrtu Arka planda Bütün O "Toplam Ciro" Formülünü İptal eder, Veriyi Saniyeesidne İzmiere Gödre Süzüp "Sihirli Bağlamda(Filter COntcext)" Yeenden Hsalar!

## Dilin Mantığı ve Kod Yapısı
Görüntsü, Excel Formüllerin Cok Benzer. (`=TOPLA() Gibi`). Ama Mantığı Excelden %100 Farkldıır.
Excelde Formül **Bir Hücreye (Örn Sütun İçndeki C2 HÜCRESİNE)** aitdir.
DAX'da FormüL Bütün Tabeloya Yaı Da **Tüm Sütuna/Matematiksl Kavrama (Measure)** Aİttidr.

Sihirli Fonkisoynuy `CALCULATE()` dir. Bütüyn YAzılan Kurrallatı(Filtrleri) Yıkarak Şartllı Yeni Topllamalr Yaaratıtı. (Adteta C++ Dakiki Pointerlarıjn Bükülmsie Ggbdiirir).

### Örnek Bir DAX Kodu: Geçen Yılın Aynı Dönemine (Time Intelligence) Göre Ciro Büyüme Oranını Milsaiynade HsapaLayan Zeka!
Power BI İçerisinde, Sirket Patronua Ekrnada KırmiziVeya Yesil Ok Göstermek icin YAziiln Muauzzaamm "Ölcü (Measure)" Formul Ciftliklerii :

```dax
/* BU BIR MS DAX (Power BI Formul/Analitik) KODUDUR */

// 1. BASIT BIIR OLCU (MEASURE) YARATMAK = TUM SATISLAAIRIN TOPLAMI (CIRO)
// (Dikkat, Exceldeki Topla(A1:A500) giib Hucre İSmi vermiyuoruz. "Sales" Tabalosnun "Amount" SutnunnU kOmplke Toopla diouzrz! )
Toplam_Ciro = SUM(Sales[Amount])


// 2. DAX'IN SIHRI (CALCULATE) => GECEN YILIN AYNI DONEMININ CIROSUNU BUL!
// Patron Sag Taraftan "Mayıs 2024" secttiyse, Bu DAX kodu arkaplandda Onu İPTALE EDiop "MAYIS 2023"'ün ToplanniiniBulraark Ekrana(Zamana Karsi) vuruurr!
Gecen_Yil_Ciro = 
    CALCULATE(
        [Toplam_Ciro],                         -- Neyi Hesseapliycaigizi(Olcumu) Verr
        SAMEPERIODLASTYEAR('Date'[Date])       -- Ne Zamnnaa Gore ? (DAx İcndeiki Sihrili "Ayni Donemin Gecen yili!" Kütüphaneis ZAMANI Vukuu ett!!)
    )


// 3. YUZDELIK BUYUMEYI HESPLA VE PATRONA(GRAFIGE) CEAK !!
Ciro_Buyume_Orani_Yuzde = 
    // Eger Adam Yeni Sirket Acmişse Ve Gecen Yil SifiRSa (Dividyeb zero ERROR Cökejmesin Dİyye DIVDIDE fonnjsyionu Safefety ypaark(Virgullu Boelr))
    DIVIDE(
        [Toplam_Ciro] - [Gecen_Yil_Ciro],   -- KAR / ZARAR Farkikkini BOLL!!
        [Gecen_Yil_Ciro],                   -- GEcan Yİlaan Böl( Yuzzdeniyih bull)
        0                                   -- Eger GECEN yil Yoksa Hata Verme 0 (Bosluk) Cek !!
    )

```
Eğr İş Zekasiı(Power BI) Aarayuzüne Bu Formüllkeri "Measure" Olarak Koyaarasani; Müşteri(SATIŞ Mudürüü) O Arayuzde Çubuklu GArafiğe Tıkllayıp "Bana Marmarayı Göserter Dediğpimde;
CALCULATE() Sihirii DEVree girer ve O "Marmara" Filtestriirni TÜM Formüllrer İcnden Geciriereeek Milyonalara SAatırlıkk Veridkedi BÜYÜMEYİ O an Saniyesyisnde Dinammikk Oldrak Çizertrrr! Exceolın YAPAMAYCGAİı Mucziee buduro.

## Kimler Kullanır?
* Finanansal Raporlkamalr, Karlılkkı AnLaiszleri Ve Şirketlerin DAmarlarındani Veriileri "Görsel Sanata(DashboardlarA)" Dökülükleek Yüklseen **Veri Analistleri (Data Analysts), İş Zekası (BI) Geliştiricileri ve FinanS Kontarollerörie**.
* Miccroosft'un Ekoositenindki Milyar Dolarlik PoweBI Piyasssanni Yüzyılıiihn En gÇüüclci Roporolama Aaraci Yapaan "Asıl Kasli" Ve "Mototrru" Dur. Sadedirce C++ yada Java Değil, Firmanan Kazianciici "Bu Matematiktsel Formüleellerr" Hesapllarrrrr!
