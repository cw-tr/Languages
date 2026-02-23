# Handlebars (ve Mustache)

## Özet
Handlebars (ve atası olan Mustache); 2009'larda ortaya çıkan "Mustache (Bıyık)" prensiplerini alıp 2010 yılında Yehuda Katz (Ember.js ve jQuery'nin çekirdek programcılarından) tarafından geliştirilen; **"Logic-less (Mantık/If-Else İçermeyen)"** Felsefesiyle HTML içerisine sadece ÇENGEL/BIYIKLI parantezlerle `{{isim}}` Saf JSON/Veri gömen, JavaScript dünyasının Evrensel ve En popüler **Web Şablonlama (Templating)** betik dilidir.

## Nedir ve Ne İşe Yarar?
Jinja2 (Python) veya PHP şablonları HARİKAYDI ama bir sorun vardı: HTML'in içine `{% if kullanici == 'Ali' %} {% for i = 0 i < 10 %} ` gibi Spagetti (Karmaşık Logic/If-Else) kodlar Yazılabiliyordu. Bu, "Tasarım (Frontend)" ile "İş Mantığını (Backend)" iğrenç bir şekilde birbirine Kurşunluyordu.

Mustache (ve Sonradan Handlebars) Dediki: **"HTML Sadece Aptal Bir Tahtadır! Gidip Html'in İçinde For-Döngüsü Veya Matematik Yapamazsınız! Ben O Özelliklerin (Logic) Hepsini Kapatıyorum (Logic-less Felsefesi!)"**
Frontend tasarımıcısı HTML'e sadece Boş Kutular(Bıyıklar) koyar `{{ yazar.ad }}`. Backendci (JavaScript) Ona Sadece SAF JSON verisi Atar. Handlebars bu verileiri Deliklere Yerleştirip HTML'i (Sıfır matematik kullanarak) Geri kusar.

**Ne İşe Yarar?**
* **Frontend ile Backend'in Mükemmel Ayrımı:** Yazılımcılar artık Template'in İçinde Kod yazamadığı(Yasaklandığı) İçin Mecburen Bütün İş Mantığını/Hesaplamayı JavaScript DOSYASINDA Yapar (Clean Code). Şablon sadece Bir "Aynadır".
* **Evrensel Taşınebilirlik (Cross-Language):** Mustache ve Handlebars O Kadar Yalındır ki; Aynı Şablon (.hbs/.mustache dosyası) Hem JavaScript, Hem Ruby, Hem Java Hem Python Tarfından İşlenebilir. Hatta (React Kullanmadan Da) Tarayıcını İçindede (Client-Side) Render Edilebiir.

## Dilin Mantığı ve Kod Yapısı
Tamamen Çift Süslü Parantez YANİ **Bıyıklar (Mustaches `{{ }}` )** üzerine kuruludur (Adını bu Sembolün Yan Yatan Bir bıyığa benzemesinden alır). Handlebars(Gidonlar) ise Bu Bıyıkların Gelişmişini(Helperlerini) Eklediği İcib bu ismi Almıştır.

İçinde Sadece `{{#if}}` Veya `{{#each}}` Olarak Block (Kapsayıcı) Basit Açmalar Vardır, Ancak (Özellikle Mustache'de) "x > 5 İse" Gibi Matematikler VEYA Mantık Taraması Kesinlikle HTML'de Yapılmaz! 

### Örnek Bir Handlebars Kodu (Mantıktan Arındırılmış Saf Veri Beslemesi)

Tasarımcının Yazdığı Pürüzsüz, Gözü Hiç yormayan `.hbs` (HTML) Şablon Mimarisi:

```html
<!-- BU BIR HANDLEBARS (.hbs) SABLONUDUR! -->

<!-- 1. Normal Değişkenler Gömme (Mustache) -->
<h1>Kullanıcı Profili: {{ profil.isim }}</h1>

<!-- Eğer Kullanıcının Biyografisi HTML ise ÜÇ Bıyık İLE (Güvenliği Ezip) GÖM: -->
<!-- (Eğer {{ }} ikili olsaydı XSS saldırısına Karşı Kodları Stringe çevirirdi!) -->
<p>Hakkında: {{{ profil.html_biyografi }}} </p>


<!-- #EACH => For Döngüsünün Saf Versiyonu (Liste) -->
<!-- Gelen JSON'daki 'arkadaslar' Dizisinin (Arrayin) İçine Gir  -->
<ul class="arkadas_listesi">
  
  {{#each arkadaslar}}
    <!-- Şu An Arrayin içindeyiz! This = Arkadaki O anki Obje -->
    <li>
         Profil Foto: <img src="{{this.fotograf_url}}"> <br>
         Adı: {{this.isim}} 
    </li>
  {{/each}}
  
</ul>

<!-- NOT Dikkat: Jinja'daki gibi {{#if arkadaslar.isim == "ali"}} YAZAMAZSINIZ!
     Mantıksal (Matematiksel Karşılaştırma) Yoktur! Siz Sadece JSON'A 
    'aliMi' Diye Bir Boolean/Şart Verisini JavaScript'te HESAPLAYIP Şablona Hazır Verirsiniz -->
```
*(Arka Plan JavaScript Kodu şöyledir)*:
```javascript
var sablon = Handlebars.compile("Yukarıdaki HTML Metni");
var veri_jsonu = { profil: {isim: "Berk"}, arkadaslar: [{isim: "Ayse", ...}] };
var bitmis_html = sablon(veri_jsonu); // EKRANA VURACAK METİN ÇIKARIR
```

## Kimler Kullanır?
* Evrendeki **Node.js (Backend JS)** Geliştiricileri ve Statik Site JS Jeneratörleri. (Örnek Express.js sunucularında Görüntüye/Ekrana Web sitesi Atmak İçin En Çok Kullanılan Şablon Motorudur HBS).
* React Ve Vue'nun Çılgınlığından (Ve Ağırlığından) Kaçıp; Saf, Eski Usul ama Hızlı Server-Side HTML üretmek İsteyen Eski toprak (Veteran) Mimarlar. "Bize Logic-Less (Aptal Şablon) Verin, Kafamız Rahat Etsin!" Diyen Front-end/Design Ekiplerinin Can simidi, Kod/Tasarım Ayrılığının Piridir.
