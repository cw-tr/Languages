# HTML (HyperText Markup Language)

## Özet
HTML (HyperText Markup Language - Hiper Metin İşaretleme Dili); 1990 yılında CERN'de Tim Berners-Lee tarafından icat edilen, Dünyadaki tüm web sitelerinin İskeletini, Temelini ve "Metin/Görsel" hiyerarşisini oluşturan, internette gördüğümüz **Her şeyin içine yerleştirildiği** Evrensel ve Tartışmasız Tek İnternet Biçimlendirme (Markup) dilidir. Kendisi bir "Programlama Dili" DEĞİLDİR; sadece tarayıcıya "Bu bir başlıktır, şu bir butondur" diyerek belgeyi resmeden kurgudur.

## Nedir ve Ne İşe Yarar?
1980'lerde bilim insanları ellerindeki araştırma belgelerini birbirlerine (İnternet/Ağ üzerinden) gönderiyordu. Ancak bu metinler düz "Notepad" yazıları gibiydi; İçinde kalın harf, resim veya başka bir belgeye zıplatan "Tıklanabilir Link (HyperText)" yoktu. Tim Berners-Lee, belgelerin içine "< >" (Küçüktür/Büyüktür) işaretleriyle bazı "Tag (Etiket)" ler koyalım dedi. 

Siz `<h1> Araştırma </h1>` yazdığınızda, Web Tarayıcısı (Browser) o etiketi gizliyor ve içindeki yazıyı Ekrana DEVASA BİR KARAKTERLE (Başlık olarak) basıyordu. İşte World Wide Web (www) bu şekilde doğdu!

**Ne İşe Yarar?**
* **Web Sitelerinin İskeleti:** Bugün Google, Facebook, Twitter veya girdiğiniz en lüks 3D Web oyunu bile; Arka planda kesinlikle ve kesinlikle bir HTML dökümanı olmak ZORUNDADIR. Tarayıcılar sadece HTML anlar. (JS ve CSS, HTML'nin içine zerk edilir/bağlanır).
* **SEO ve Semantik (Anlamsal Yapı):** Google'ın Arama motoru robotları (Örümcekleri) sitenize girdiğinizde tasarımınızı göremez. HTML içindeki `<title>` (Başlık), `<main>` (Ana Gövde), `<article>` (Makale) etiketlerini okuyarak sitenizin ne hakkında olduğunu anlar.

## Dilin Mantığı ve Kod Yapısı
Dediğimiz gibi; HTML, içinde IF-ELSE (Eğer-İse) veya "Şu sayıyla bunu çarp" (Döngü/Matematik) barındırmaz. Bu yüzden ona "Programlama Dili" diyenlerle yazılımcılar topluluğunda sertçe alay edilir (Klasik bir şakadır). 

HTML, tamamen İç İçe Geçmiş Hiyerarşik Kutulardır (DOM - Document Object Model). 
Hemen hemen her Tag'in (Etiketin) bir Açılışı `<tag>` ve Bir Kapanışı `</tag>` (Eğik çizgi) olmak zorundadır. Etiketlerin içine Sınıf (Class) veya Kimlik (ID) gibi **Attribute (Nitelik)** eklenebilir.

**Örnek İşleyiş (Sembolik Olarak):**
Masaüstü Programlama (C#) : `buton1.Text = "Tıkla"; buton1.Width = 100; mainWindow.Add(buton1);`
HTML Web: `<button class="kirmizi-buton"> Tıkla </button>` (Sadece metin olarak beyan edersiniz, Tarayıcı Çizer).

### Örnek Bir HTML Kodu: Modern HTML5 Şablonu
Günümüzde bir web sitesi tasarlanırken kod dosyasının (`index.html`) içine yazılan o asil, standart temel İskelet:

```html
<!-- HTML Dilinde Yorum Satirlari bu garip Ok ( <!-- --> ) formatındadır -->

<!-- 1. BEYANNAME: Tarayiciya "Kanka bu eski HTML4 degil, Yeni HTML5'tir!" diyoruz -->
<!DOCTYPE html>

<!-- HTML ETIKETI (KOK/ROOT): Butun evren bunun icine yazilir -->
<html lang="tr">
  
<!-- YUKARI KISIM (HEAD): Ziyaretcinin GOREMEDIGI arka plan, SEO ve Ayar kodlari -->
<head>
    <!-- Trkce Karakterlerin (Ş,Ğ,İ)  Bozulmadan cikmasi icin Evrensel UTF-8 Kodu: -->
    <meta charset="UTF-8">
    <title>Benim Harika Sitem</title>
    <!-- Css baglamak veya Ikon secmek burda yapilir -->
</head>

<!-- GOVDE KISMI (BODY): Ziyaretcinin Ekranda GORDÜGÜ Kisim (Yazilar, Butonlar) -->
<body>

    <!-- Baslik Etiketi (Heading 1 - En buyuk baslik) -->
    <h1>Hoş Geldiniz!</h1>
    
    <!-- Paragraf Etiketi (Paragraph) -->
    <p>Bu site, HTML in nasil bir <b>kemik / iskelet</b> oldugunu anlatan sihirli bir sayfadir.</p>
    
    <!-- Liste Yapisi: Unordered List (UL) ve icinde List Item (LI) -->
    <ul>
        <li>HTML: İskelettir.</li>
        <li>CSS: Kaslar ve Deridir (Makyajdir).</li>
        <li>JavaScript: Beyin ve Sinir sistemidir (Hareket ettirir).</li>
    </ul>

    <!-- Dünyayı (İnterneti) Birbirine Bağlayan Link (Anchor/A) Etiketi! -->
    <!-- href (Hedef Referansi) niteligi nereye gidilecegini gosterir: -->
    <a href="https://www.google.com" target="_blank">Google'a Zıpla!</a>
    
    <!-- Gorsel/Resim Cekme (Kapatici etiketi(</img) olmayan nadir tekil etiketlerden (Image)) -->
    <img src="kedi_resmi.jpg" alt="Sevimli bir kedi">

</body>
</html>
```
Bu kodu boş bir Not Defterine yazıp uzantısını `.html` olarak kaydedip "Çift Tıkladığınızda"; bilgisayarınızdaki Google Chrome derhal açılır ve ekranda (İnternet olmasa da, lokalden) büyük başlığı ve altında Mavi Linkiyle harika bir web sayfası (Tasarımı Çirkin/Sade bir sayfa) gösterir. Yaratım gücü çok daktilomsudur.

## Kimler Kullanır?
* Evrendeki bütün **Front-End (Önyüz) Web Geliştiricileri**, UI/UX Tasarımcıları.
* Sadece modern React veya Vue çerçeveleri(Framework) yazsanız da, o "süslü" JS kodunuzun arkaplanda Tarayıcıya zorunlu fırlattığı derlenmiş Metin yine (DOM'a eklenen) "HTML" elementleridir (Div, Span).
* Bir metin yazarı/blogcuysanız (WordPress kullanırken bile) arka plandaki sekmeye geçip "Ya şuraya bir H2 Başlığı atalım, veya Strong(Kalın) etiketine sokalım" diye HTML ile müdahale ederseniz internette hayatta kalırsınız. "Hello World" ten bile kolay, Eğlenceli ve her insanın temel dijital kütüphanesidir.
