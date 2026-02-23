# JavaScript (JS)

## Özet
JavaScript (JS); 1995 yılında Brendan Eich tarafından sadece 10 günde, web sayfalarına "Canlılık" (tıklanan butonlar, uçan resimler) katmak için yaratılmış, yıllar içinde mutasyon geçirerek bugün **Masaüstü, Mobil, Sunucu ve bütün İnternetin yegâne görsel dili** haline gelen ve Evrenin En Çok Kullanılan Programlama Dilidir.

## Nedir ve Ne İşe Yarar?
1990'ların ortalarında internetteki siteler (HTML ve CSS ile yapılanlar) sadece bir Gazete kağıdı gibi okunabilen, tepki vermeyen ölü sayfalardı. Kullanıcı bir Forma ismini yanlış girdiğinde (Örn: Boş bıraktığında), sayfa bunu anlayamaz, sunucuya (Java/PHP'ye) gönderir, sunucu saniyeler sonra "Boş giremezsin" diye sayfayı baştan yüklerdi.

Netscape şirketi, "Tarayıcının (Google Chrome / Internet Explorer) içinde çalışacak, sunucuya gitmeden kullanıcının bilgisayarında hemen o saniye butonları oynatacak, hataları yakalayacak küçük bir dil lazım" dedi. Adını o dönem Java çok popüler olduğu için pazarlama hilesi olarak "JavaScript" koydular (Fakat dil olarak Java ile akrabalığı "Hamam böceği ile Hamam" kadardır, hiç alakaları yoktur). 

Ancak 2009 yılında Ryan Dahl, JS'yi Chrome tarayıcısının içinden söküp "Node.js" adıyla Arka Plan (Sunucu/Backend) İşletim sistemine entegre edince, JavaScript dünyayı tamamen fethetti.

**Ne İşe Yarar?**
* **Tüm Gezegenin Ön Yüzü (Frontend / UI):** YouTube'da bir videoyu durdurduğunuzda, Instagram'da aşağı kaydırdıkça ekranın donmadan yeni resim getirmesinde (AJAX), React/Vue.js gibi devasa kütüphanelerin içinde %100 oranında JavaScript motoru çalışır. Bütün Front-end (Ön yüz) geliştiriciler Javascript bilmek, JS yazmak ZORUNDADIR. Rakibi yoktur.
* **Modern Sunucular (Node.js Backend):** Ön yüz için JS öğrenen mühendisler, Node.js sayesinde veritabanı(MongoDB) ve sunucu komutlarını (Express.js) da aynı dille yazmaya başladılar. Böylece bir programcı "Full-Stack (Her Şeyi Yapan)" mühendise dönüştü.
* **Masaüstü (Desktop) Uygulamalar:** Bilgisayarlarımızda yüklü olan Discord, Spotify, Visual Studio Code (VSC) ve Slack... Bunların HİÇBİRİ C++ veya Java ile yazılmamıştır! Hepsi, gizli bir tarayıcı penceresi açıp içinde Javascript çizen (Electron.js) altyapısına sahiptir.

## Dilin Mantığı ve Kod Yapısı
Nesne Yönelimlidir ama C# veya Java gibi keskin Sınıflardan (Class) ziyade **Prototip Tabanlı (Prototype-based)** çok esnek ve çok tehlikeli kalıtsal kuralları vardır. 

**En zayıf Yönü ve En Büyük Gücü: "Dinamik/Zayıf Tiplilik".** 
JS'te hiçbir şeyin tipini söylemezsiniz. Bir değişkene önce Sayı (5), sonra Metin ("A"), sonra Dizi(Array) atayabilirsiniz, sistem çökmez. Dahası dil o kadar çılgındır ki: Python'da `5 + "5"` yaparsanız "Sayıyla Metni toplayamazsın" diye sistem kırmızı alarm verip durur. JavaScript çalışmaya devam etmeye Yeminlidir: O yüzden "Sayı olan 5'i gizlice Metne(String) dönüştürürüm" der ve sonucu `55` olarak yanyana yapıştırıp size verir! Programınız çökmeksizin sessizce çalışır (Ama milyon dolarlık banka bakiyeniz 10 yerine "55" yazdığı için siz çökersiniz).

**Örnek İşleyiş (Sembolik Olarak):**
Asenkron çalışmanın (Aynı anda bloke olmadan iş yapmanın) krallarındandır. Siz veritabanından 1 GB resim getirmesini istersiniz `fetch()`, JS bunu beklerken asla ekranı veya mouse'u kitlemez (Non-Blocking). Arkada `Event Loop` adındaki küçük ve devasa otoyol çarkı döner durur. Vakit geldiğinde (Veri inince) ekrana (Callback/Promise) fırlatır. "Event-Driven (Olay Odaklıdır)".

### Örnek Bir JavaScript Kodu: Arayüz Sihri (DOM) ve Asenkron İnternet
Herhangi bir kurulum gerektirmeyen (F12 ye basıp Chrome'da bile yazılan) Web sayfasına hükmetme ve bir butona tıklandığında Sayfayı donmadan sunucuyla görüştürme (Asynchronous) harikası:

```javascript
/* JS dilinde çoklu yorum satırları (/*) ile veya tek ise (//) ile gösterilir */

/* 1. ADIM: Ekranda (HTML sayfasında) var olan <button id="uyeOl"> objesini, 
   Düz metinden çıkartıp (C++ içindeki DOM motoru sayesinde) JAVASCRIPT Nesnesine cevir / Bağla! */
const kayitButonu = document.getElementById("uyeOl");

/* 2. ADIM: Olay Dinleyicisi (Event Listener). 
   JS Asla sayfayi veya Tıklamayi beklemez! Butona "Sana basilana kadar arkada uyu, Basilinca(click) şu foksiyonu tetikle" der. */
kayitButonu.addEventListener("click", ayniAndaBirdenFazlaIslemYap);


/* === ES6+ Modern, Asenkron (Gecikmesiz) Sınıfı-Andıran Kod Bloğu === */

// "async" kelimesi, bu fonksiyonun içindeki emirlerin Interneti/sunucuyu bekleyecegini,
// Bilgisayarı/Kullaniciyi Asla Dondurmamasi (Lag yapmamasi) gerektigini Event Loop'a emreder.
async function ayniAndaBirdenFazlaIslemYap() {
    
    // JS'in esnek degisken tanımı "let" (sonradan degisebilir):
    let gelenKullanici = "Ahmet Can";
    
    console.log(`${gelenKullanici} arka planda kaydoluyor... (Ekran donmadi!)`);
    
    try {
        // "await" Sihri: JS bu satirda durur ve SUNUCUYU bekler. 
        // Ancak mouse oynamaya, sayfada gezmeye devam edersiniz (Thread kitlenmedi!).
        // Arka Planda baska bir sunucuya (API) HTTP istegi atiyoruz:
        
        let sunucuYaniti = await fetch("https://api.ornek.com/yeniKullaniciOlustur");
        
        // Eger basarili dönerse (200 OK Status), JSON (JS'nin kendi Orijinal Formatina) Metni Çevir: 
        let veritabaniDatasi = await sunucuYaniti.json();
        
        // Ekrana (Console'a degil direkt Kullanicinin Html yuzune DOM ile bas)
        alert("Kayit Basarili! Hosgeldin " + veritabaniDatasi.isim);
        
    } catch (hata) {
        
        // Coktu mu? İnternet mi koptu? Asla Kirmizi ölüm mesaji çikarma, Temizce buraya düş.
        console.error("Malesef baglanti kurulamadi: ", hata);
        alert("Sunucu çöktü canım, sonra gel.");
        
    }
}
```

Evrensel internet bu kadar basit olan ama arkasında dünyanın en zeki C++ derleyicilerinden biri (Google V8 Engine) koşan bu ufaklık tarafından domine edilmektedir.

## Kimler Kullanır?
* Başlangıç seviyesindeki her yazılımcı (Çünkü tarayıcı ve F12 dışında kurulum istemez).
* React, Vue, Angular kullanan bütün Front-end (Arayüz / Tasarım Geliştirme) mühendisleri. Evrendeki Webmaster'lar.
* Node.js, Express.js kullanan sunucu (Backend) mimarları. 
* React Native sayesinde yazılan Instagram, Discord, Skype gibi platformları (Aynı kodla hem IOS hem Android) tek bir hamlede kodlayan Mobil Geliştiriciler. 
* Kısacası JS, Web Programcılığının (Efsanevi) Havası ve Suyudur. Ne onla tam olunur, ne de onsuz yaşanır.
