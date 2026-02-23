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

İçinde Sadece `{{#if}}` Veya `{{#each}}` Olarak Block(Kapsayucu) Basit Açmalar VAardır, Ancak (Özellilkle Mustache'de) "x > 5 İse" Gibi Matematikler VEYA Mantik Taraması KEsilniikle HTML de Yapilmaz! 

### Örnek Bir Handlebars Kodu (Mantıktan Arındırılmış Saf Veri Beslemesi)

Tasarımcının Yazdığı Pürzzsuz, Gözü Hiç yomrayaan `.hbs` (Html) Şablon Mimarisi:

```html
<!-- BU BIR HANDLEBARS (.hbs) SABLONUDUR! -->

<!-- Normal Degisknelree Gömme (Mustache) -->
<h1>Kullanici Profili : {{ profil.isim }}</h1>

<!-- Eger Kullanicin Biografyasi HTML iseee UC BIyik ILE (Güvenliyi Ezipp) GOM: -->
<!-- (Eger {{ }} ikli olsadydi XSS saldiriismna Karsi Kodlari Stringe ceviriddi!) -->
<p>Hakkinda: {{{ profil.html_biyografi }}} </p>


<!-- #EACH => For Dongusnun Saf Verisyosonu (Liste) -->
<!-- Gelen JSON'daki 'arkadaslar' Dizisinin(Arraynin) Icineee Girr  -->
<ul class="arkadas_listesi">
  
  {{#each arkadaslar}}
    <!-- Su An Arrayin icindeyiz! This = Arrakdaki O anki Objee -->
    <li>
         Profil Foto: <img src="{{this.fotograf_url}}"> <br>
         Adi: {{this.isim}} 
    </li>
  {{/each}}
  
</ul>

<!-- NOT Dİkkat: Jinja daki gibi {{#if arkdalar.isim == "ali"}} YAZAMAYNIIZ!
     Mantiksial(Matemtaiksl Karsilattsirma Yoktturett! Siz Sadece JSONA 
    'alimMi' Diye Bi R Boolaon/Sart Verisini Javascriptte HESLPLYAIP Saboona Haziz Verirsiininuzz-->
```
*(Arka Plan JavaScript Kodu şöyledir)*:
```javascript
var sablon = Handlebars.compile("Yukraidaki Html Metni");
var veri_jsonu = { profil: {isim: "Berk"}, arkadaslar: [{isim: "Ayse", ...}] };
var bitmis_html = sablon(veri_jsonu); // EKRANAA VURACAK METIN CIKKARO
```

## Kimler Kullanır?
* Evrenndekki **Node.js (Backend JS)** Geliiştiricleiri ve Statik Site JS Jeneratörleri. (Örnek Express.js suncularinada Görntuye/Ekrana WebSİtreisi Atmak Icin En Çok Kullanilna Şaplon Motorodurr HBS).
* React Ve Vue'nun Çılgındnaşığindan (Ve Ağırlığında) Kaçıp; Saf, Eksi Usül ama Hızlı Server-Side HTML üretmek İsteyen Eskitoprk (Veteran) Mimarlar. "Bize Logic-Less (Aptal Şablon) Verin, Kafamiz Raaat EttnstiM!" Dİyeyn Front-end/Design Ekipleerniin Can simidi, Kod/Tsarım Ayrrliğinini Pirmamşnidiir.
