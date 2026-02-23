# ColdFusion (CFML)

## Özet
ColdFusion (ve onun eklentili dili CFML - ColdFusion Markup Language); 1995 yılında Allaire Kardeşler tarafından (sonrasında Adobe firması ve Macromedia tarafından satın alınan) icat edilmiş, insanların o dönemki C++'ın arka plan sunucu (Back-End) eziyetiyle uğraşmadan, sanki "HTML yazıyormuş gibi Etiketlerle (Tags - `<cf...>` )" doğrudan Veritabanına (SQL) bağlanan Web Siteleri yaratmasını sağlamış devrimsel Kurumsal Web-Ara yazılımı ve dildir.

## Nedir ve Ne İşe Yarar?
1995! Javascript daha yeni doğmuş, PHP ortalıkta (işe yarar şekilde) yok, ASP.NET on yıl sonra çıkacaktı ve sunucu arka planındaki C++ dili (CGI scriptleri) ile Web Sayfasına bir şeyler veritabanından yazdırmak "Bilgisayar Mühendisliği" derecesi istiyordu. 

J.J. Allaire dedi ki: "Neden düz metin yazan Web (HTML) Tasarımcılarına küçük bir kıyak yapmıyoruz? Eğer sayfasının içine `<cfquery>` diye sanki HTML paragraf etiketi gibi bir etiket açarlarsa, bunu bizim kurduğumuz özel web-sunucusu (Yani ColdFusion motoru) anlasın, içindeki o etiketi Veritabanına yollasın, sonra HTML sayfasını kullanıcıya Basıp versin!" İnanılmaz şekilde patladı! Çünkü bir çocuk bile Web sunucusundan Veri Cekebiliyordu artık (Rapid Web Development).

**Ne İşe Yarar?**
* **Dinamik Veri Destekli Web Siteleri:** 1990 sonu ve 2000'lerde, İnternette Form doldurabildiğimiz, Sipariş verdiğimiz e-ticaret sitelerinin ve Büyük Kurumsal Hükümet/Tıp websitelerinin arkaplan motoru (Sunucu mimarisi) olarak dünyayı eline geçirmiştir. (ÖRN: Amerikan savunma bakanlığı ve eski Myspace).
* **Günümüzde Kurumsal İntranet (Legacy):** Eski Amerikan üniversitelerinde ve iç işleyiş otomasyonlarında Adobe ColdFusion Serverlar arkada kurulmaya ve bu tag (etiket)'lerle hizmet (PDF formatı çıkarma, Excel basma vb) yollamaya devam eder.

## Dilin Mantığı ve Kod Yapısı
Temel dili **CFML (ColdFusion Markup Language)**'dir. Yapısı itibariyle Programlama Dili VEYA Nesne dili değil; baştan aşağı XML/HTML etiketlerinden ibaret olan bir "İşaretleme Şablon (Markup Template Engine)" dilidir. Bir dosyanın (.cfm) içine dümdüz HTML etiketleri (`<h1>`, `<div>`) atarsınız, araya da `<cf...>` ile başlayan ColdFusion etiketlerini çakarsanız. Sunucu bu `<cf..>` leri ayıklar, eyleme (SQL vb) dönüştürüp çıktıyı HTML yaparak kullanıcıya web sitesini hediye eder.

Fakat, insanlar 2000 sonrasında "Ben algoritmayı ve If Else döngüsünü `<cfif>` tagı diye kapatıp HTML gibi uzun yazmaktan iğreniyorum" dediğinde Adobe ColdFusion; Tag fantezisini çöpe atıp Javascript'e/Java'ya inanılmaz benzeyen **`CFScript`** adlı ikinci bir kod mimarisi çıkardı kodun ortasına. (Sayfanın ortasına script yazar gibi kodlamayı sağladı). Lakin ruhu hep "Tag" formudur.

**Örnek İşleyiş (Sembolik Olarak):**
PHP Kodu (Kısalık felsefesi): `<?php echo "Mevcut Gun: " . date("Y/m/d"); ?>`
ColdFusion Kodu (HTML Tag Felsefesi): `<cfoutput> Mevcut Gun: #DateFormat(Now(), "YYYY/MM/DD")# </cfoutput>` (Buradaki `#` (Diyez) sembolü HTML'nin içindeki o Değişkenin / Methodun Ekran çıktısına vurulmasını (Output) temsil eden Coldfusionun yegane can damarıdır!).

### Örnek Bir ColdFusion Kodu: Tıklamayla HTML içine SQL Bagıntı (Query) Enjeksiyonu!
1997'de bir tasarımcıya bile Veritabanı hackerı muamelesi çektiren O efsanevi (Günümüzde artık Terk Edilen ama saygıyla anılan) CFML Tag Engine mimarisi. (Veritabanından Haberleri Çekip Listeleme):

```coldfusion
<!-- Standart HTML Sayfa Baslangici -->
<html>
<head>
    <title>Sirket Haberleri!</title>
</head>
<body>

    <h2>Bu ayki Guncel Calisan Haberleri</h2>
    
    <!-- 1. CF MUCİLESİ: SERVER'IN KALBİNE BIR ETIKET AÇ (<cfquery>) -->
    <!-- Name=Sorgunun_Ismı (Bunu Asagida Dongu icin kullanicaz!),  datasource: Sqlveritabani_Ip_Ayarimizin Ismı! -->
    
    <cfquery name="HaberSorgusu" datasource="SirkeyOracleVeritabani">
        
        <!-- YALNIZCA KURALSIZ, DUZ SAF SQL YAZ! ARkasi CF'ye Aittir: -->
        SELECT HaberBasligi, Yazar, YayinlanmaTarihi 
        FROM IsciHaberleri_Tablosu 
        WHERE Durum = 'AKTIF'
        ORDER BY YayinlanmaTarihi DESC
        
    </cfquery>
    
    <!-- CFQUERY Bitti, Database Bitti, Hafizada Degisken ('HaberSorgusu') Olarak asili kaldi! -->

    <ul>
    
    <!-- 2. EKRANA ÇIKTI(HTML) BASMA DÖNGUSÜ (<cfoutput>) VEYA (<cfloop>) -->
    <!-- "Basariyla donen Query/Sorgu degiskenimizkadar (5 mi? 10 mu?) Asagıdakini RECURSİVE Kopyala " -->
    
    <cfoutput query="HaberSorgusu">
        
        <!-- DİYEZ (#) İŞARETİ COBDFUSİON İÇİN RAM'DEKİ/SQL'DEKİ DEĞİŞKENİ TEMSİL EDER! -->
        <!-- Sütun Isimlerini Diyezlerin Icine Yaziyoruz: -->
        <li>
            <b>#HaberBasligi#</b> <br/>
            Yazan: <i>#Yazar#</i> | Tarih: #DateFormat(YayinlanmaTarihi, "mm/dd/yyyy")#
        </li>

    </cfoutput>
    
    </ul>
    
    <!-- Eger Hic Haber Bulunmadiysa (If Kontrolu - Tag olarah!) -->
    <cfif HaberSorgusu.RecordCount EQ 0>
       <p style="color:red;">Henuz Hic Haber Girilmemiş.</p>
    </cfif>

</body>
</html>
```
Bu sayfa Internet Explorer üzerinden ziyaret edildiği zaman ne `cfquery` nede `cfoutput` HTML'ye yansımazdı. Adam sağ tıklayıp kaynağı görüntüle dediği zaman sadece pürüzsüz "Haber1, Haber2" isimli `<li>...</li>` HTML liste elemanları görürdü. Server bütün pis arka planı o Tag'lar sayesinde yutardı!

## Kimler Kullanır?
* 2020'ler dünyasında açıkçası **HİÇ KİMSE (Yeni projelerde) KULLANMAZ**. Çünkü PHP (Bedava ve çok pratik olmasıyla), sonra ASP.NET (Microsoft kurumsallığıyla) ve Javascript sunucuları (NodeJS) Coldfusion'un tüm yapabildiği Web Arka-Plan işini ele geçirmiştir. (Adobe Coldfusion Server çok ciddi Lisans Ücretleri Kesiyordu).
* Günümüzde kullananlar sadece; 2000'li yıllardan kalma Devlet/Belediye Web Sitelerinin Milyonlarca Datasını barındıran o eski HTML/CF sitelerini (Çökmesin ve ayakta kalsın diye) revize eden "Adobe ColdFusion Form (Lucee vb.)" Geliştiricileri ve Kurumsal Amerikan İntranet Legacy (Miras/Ölü) Bakım Teknisyenleridir.
