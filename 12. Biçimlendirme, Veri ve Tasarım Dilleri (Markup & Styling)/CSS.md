# CSS (Cascading Style Sheets)

## Özet
CSS (Cascading Style Sheets - Basamaklı Stil Şablonları); 1996 yılında W3C (World Wide Web Consortium) tarafından icat edilen, HTML'in o renksiz, şekilsiz ve alt-alta yığılan dümdüz (Word belgesi gibi) siyah/beyaz iskeletini; **Muazzam renklere, Kutulara (Grid/Flexbox), Yuvarlak köşelere, Gölgelere ve modern Animasyonlara (Hover vb)** boyayan internetin yegane "Görsel, Tasarım ve Makyaj (Styling)" dilidir. HTML gibi bu da bir Algoritma/Programlama dili değildir!

## Nedir ve Ne İşe Yarar?
1994'lerde web siteleri sadece HTML'den oluşuyordu. İnsanlar bir "Başlığı (h1)" kırmızı yapmak istediğinde, HTML'in içine çirkin bir `<font color="red">` etiketi yazmak zorunda kalıyordu. 1000 sayfalık bir web sitesinde logonun rengini değiştirmek isterseniz 1000 dosyaya tek tek girip `<font>` etiketini silmeniz lazımdı.

Håkon Wium Lie (W3C), "Neden Görselliği(Estetiği) ve Yapıyı(HTML) birbirinden ayırmıyoruz?" dedi. Sitenin Tasarım Kodları tek bir `.css` (Stil) dosyasında tutulacaktı. HTML sadece "Benim adım H1" diyecek; CSS ise yüzlerce metre uzaktaki dosyadan "Hey tarayıcı (Browser)! Sitedeki Bütün H1'leri Gök Mavisine boya ve Sağa Hizala!" diye bağıracaktı. Tasarım ve Verinin birbirinden izole edilmesi Web'in Çehresiydi.

**Ne İşe Yarar?**
* **Responsiveness (Duyarlı / Mobil Uyumlu Tasarım):** Eğer sitenize Cep Telefonundan (`@media screen`) giriliyorsa yan yana duran menüleri Otomatik Olarak alt-alta Burger (Hamburger) Menüye çeviren, Sayfa genişliğine göre elementleri saklayıp/uzatan tek zırhtır. Kötü CSS bir siteyi harabeye, İyi CSS o siteyi "Apple.com" kalitesine çevirir.
* **Görsel Algı (UI/UX) ve Animasyonlar:** Butonun üstüne fareyle gelince butonun (0.3 saniyede) yavaşça parlaması veya yana doğru süzülen Slide(Kaydırma) bannerları 2014 sonrası CSS3 güncellemesi ile tamamen JavaScript'e yük olmadan Pürüzsüz GPU hızıyla CSS üzerinden çizilir.

## Dilin Mantığı ve Kod Yapısı
Tam bir "**Seçici (Selector) ve Kural (Rule)**" sözlüğüdür. Kimi boyayacağını seçersin `{}`, içine kuralını `Özellik: Değer;` olarak iki nokta üst üste ile (Key:Value) atarsın.

1. **Tag (Eleman) Seçici:** Sadece ismini yaz `button { }` (Sitedeki her butonu boyar).
2. **Klass (Sınıf) Seçici:** Başına Nokta (.) koy `.uyari-yazisi { }` (HTML'de class="uyari-yazisi" olanları Özel olarak boyar).
3. **ID (Kimlik) Seçici:** Başına Diyez (#) koy `#ana-baslik { }` (Sayfada sadece Tek Bir Özel Elemanı boyar).

Adındaki "Cascading (Basamaklı)" felsefesi: Aşağı satırdaki yazılan kodun, Yukarıdakini "Ezmesi (Override)" prensibidir! Renkleri çelişirse, En Son okunan emir geçerli sayılır (Hiyerarşi silsilesi). 

**Örnek İşleyiş:**
HTML'deki Kodun: `<p class="haber">Ali geldi</p>`
Bunun CSS Dosyasındaki Karşılık (Boya) Emiri: `.haber { color: red; font-size: 20px; }`

### Örnek Bir CSS Kodu: Estetik Bir Buton Çizmek ve Hover Animasyonu
Girdiğiniz janjanlı E-Ticaret / Modern Web sitelerinde o farenin altında süzülen muazzam "Cam/Yumuşak" butonların arkaplan sırrı: 

```css
/* CSS Dilinde Yorum Satirlari Slas Yildiz (/* */) Şeklindedir. */

/* BODY (Evrensel Vücut): Tum siteyi hedef Al! */
body {
    background-color: #121212; /* Arkaplani Koyu Siyah/Kullu (Karanlık Mod) boya! */
    font-family: 'Segoe UI', Tahoma, Arial, sans-serif; /* İsletim sistemine gore yazi tipi degistir! */
    margin: 0; /* Tarayicinin Kenardaki cirkinn beyaz bosluklarini SIfirla ez! */
    
    /* İcerigi ortalamak icin Modern FLEXBOX gucu! */
    display: flex;
    justify-content: center; /* Yatay Ortala */
    align-items: center;     /* Dikey Ortala */
    height: 100vh;           /* Ekranin Tamami(100 Viewport Height) kadar uzan */
}

/* 2. CLASS SECİCİSİ: Nokta ile baslayan '.sihirli-buton' ozel tasarimi! */
.sihirli-buton {
    /* Butonun Kutu(Box) Tasarimi: */
    background-color: #6200EA;  /* Mor renkli Arkaplan (#HEX Kodu) */
    color: white;               /* Yazisi Bembeyaz */
    padding: 15px 32px;         /* Yazinin etrafindaki IC BOSLUK (Et etrafi) : Ust+Alt 15px , Sag+Sol 32px */
    font-size: 18px;            /* 18 Piksel Yazi Buyuklugu */
    font-weight: bold;          /* Kalin Yazi */
    
    /* Buton Cercevelerini (Kenarlıkları) sil ve Yuvarlat! */
    border: none;               
    border-radius: 8px;         /* Kenarlar C jilet gibi keskin olmasin 8 piksel ovallessin (Apple Stili) */
    
    /* Ekranda fiyati vurgulamak icin altina Gokkusagi GOLGESI ciz (X_kayma, Y_kayma, Blur_Dagilimi, Renk) */
    box-shadow: 0px 4px 15px rgba(98, 0, 234, 0.4); 
    
    cursor: pointer;            /* Fareyle ustune gelince ok, 'Zıplayan el(El_Isareti)' olsun! */
    
    /* ANIMASYON GECISI (TRANSITION) ZEKASI!: Eger bu butona bir hal/durum deigimi gelirse
       Aniden (0 saniyede Cat!) diye deismek yerine, Degisimi 0.3 Saniyeye yayarak süzül (Ease)!
    */
    transition: all 0.3s ease; 
}


/* 3. HOVER (PSEUDO-CLASS / SAHTE DURUM) MÜHÜRÜ! (Tarayicida Magic!) */
/* Mouse ".sihirli-buton" un Uzerine Bindigi (HOVER durumu tetiklendigi)  ANDA Bu kurallari oku! */
.sihirli-buton:hover {
    
    background-color: #7C4DFF; /* Rengi biraz Acik Mor/Parlama yapp */
    
    /* Buton Zzıpılamak iin TRANSFORM KORDİNATLARİNIN (Scale) Boyunu %5 bbuyut (1.05) ve Yukarı(Y eksenı) 3 px tasi (-3px)  */
    transform: translateY(-3px) scale(1.05);
    
    /* Gölgeyi daha geniş ve belirigin yap ki butun Ekranda Havalanmis gibi sussun! */
    box-shadow: 0px 8px 25px rgba(98, 0, 234, 0.7);
}
```

Bu kod HTML içine (`<link rel="stylesheet">` ile) yüklendiği ve HTML'ye `<button class="sihirli-buton">`yazıldığı saniye; o siyah beyaz sıkıcı 90'lar web siteleri bir anda Neon-Cyberpunk parıldayan devasa Apple/Netflix UI'ına(User Interface) evrimleşir. Sihir sadece (Cascading) katmanlaşmaktadır.

## Kimler Kullanır?
* Yine ve Mutlaka Evrendeki Tüm **Front-End (Önyüz/Arayüz) Mühendisleri ve UI(Arayüz)/UX(Kullanıcı Deneyimi) Tasarımcıları**.
* Günümüzde Vanilla (Saf) CSS yazmak dosya kalabalığı yarattığı için programcılar **SASS/SCSS** denilen (İçinde if/else, döngü yapabildiğiniz sahte CSS-Programlama dillerine) VEYA **TailwindCSS / Bootstrap** denen (Siz CSS yazmayın klas atın hazır fırlasın) kütüphanelere bağımlıdır. Fakat o devasa Frameworklerin hepsi, Chrome'un kalbine düşerken (Compile olduğunda) yine kusursuz SAF/ÇIPLAK CSS (.css) metinlerine dönüşmek ZORUNLULUĞUNA mahkumdurlar. Çarkın dişlisidir.
