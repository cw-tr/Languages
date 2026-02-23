# Flash (Platform & ActionScript)

## Özet
Adobe Flash (Eski adıyla Macromedia Flash); 1996 yılında doğan, 2000'li ve 2010'lu yıllar boyunca Evrensel İnternetin **Tek ve Mutlak İnteraktif Multimedya/Oyun/Video Hükümdarı** olan, Web tarayıcılarına Yüklenebilen ufak eklentilerle (Plugin) Vektörel Animasyonlar `.swf` oynatan; lakin Steve Jobs'ın Apple (iPhone) daki HTML5 devrimi ve Güvenlik Zafiyetleriyle Darbe yiyerek **2020 Yılında Resmi Olarak Öldürülen(Fişi Çekilen)** Efsanevi Canlı Tarih platformudur. 

*(Not: Dili Aslında Seviye 9'da Belirttiğimiz ActionScript'tir, Ancak Flash bir Bütünleşik IDE / Render Motoru (Platform) olarak Teknolojilerin Atalarındandır.)*

## Nedir ve Ne İşe Yarar?
1998'lerde Web siteleri "Mavi Linkli Yazılardan" ibaretti. Bir WebSitesinde Düğmeye bastığınızda Silah Sesi Duymak, havaya zıplayan komik çöp adam animasyonları İzlemek veya **YouTube'in Ta İlk Açıldığı Günden İtibaren Ekranda Video (MP4) Oynatabilmek** İnternetin Kendi yapısında (HTML/JS) İmkansızdı. (HTML5 icat edilmemişti).

Macromedia/Adobe "Flash Player" Eklentisini yaptı. İnternet Kullanıcılarına dedi ki "Şu 2 Megabaytlık Yamayı Kur!". Ve BAM! Bir anda Bütün İnternet sitelerinin ortasındaki Siyah Kutuda; ÇöpAdam Oyunları (Kraloyun/Miniclip), Sesli Chat siteleri, MSN Messenger Animasyonları belirdi. Zamanında İnternet demek; Oynatılıyorsa "Flash" Demekti.

**Ne İşe Yarar?** (Ölmeden Önceki Krallığı):
* **Flash Web Oyunları:** 2000 ile 2015 Arası Tarayıcı üzerinden Oynanan (FarmVille, Candy Crush(Facebook Ekrani), Çöp Adam Savaşları, Tower Defense) oyunlarının %99,99'u Flash (ActionScript) diliyle Vector(Hiç piksellenmeden Büyüyen Kusursuz) çizimlerle yapılıyordu.
* **Müzik ve Video Oynatıcıları:** Sitenize MP3 gömmek veya Youtube'dan Video Oynatmak için kullanılıyordu. Yıllarca bütün Youtube / Vimeo altyapısı bu Kütüphaneyi Sömürdü. 

## Dilin Mantığı ve Kod Yapısı
Flash'ın Yazılımcı (Developer) tarafındaki Görünümü Dümdüz bir **Timeline (Animasyon Montaj Ekranı)** ydı. Premiere Pro veya After Effects Arayüzü gibi Frame (Kare kare) saniyeler akar; Tasarımcı Faresiyle 10. Frame'e Bir Adam Çizer Adamı ilerletirdi. Tween (Animasyon Arakatlaması) yapardı.

Lakin o Karakteri Mouse (Keyboard) Tuşlarıyla Yönetmek istendiğinde; İşin içine Yazılım Girdi: **ActionScript (AS2.0 ve AS3.0)**. 
ActionScript 3; tamamen ECMAScript standardında yapısını Kuran (JavaScript ve C#'ı andıran), Mükemmel derecede Sıkı Objeye-Yönelik (Class, Public/Private) yapıya sahip bir Şaheserdi. 

**Örnek İşleyiş:** Zaman Çizelgesinde (Timeline) "Play" Tuşu nesnesini çizerdik (Adi: btn_oyna). Sonra O tuşa sağ tıklayıp "Action (Kod Yaz)" der ve AS kodu fırlatırdık. 

### Örnek Bir ActionScript 3.0 (Flash) Kodu: Efsanevi Flash Oyun Mantığı
Ekranda (Sahnedeki/Stage) Duran bir Oyuncuyu (Örn: Çöp Adam) Sağ(Right) Yön tuşuyla hareket ettiren O 2006 Kokulu AS3 Kodu:

```actionscript
/* FLASH SAHNESINDE YAZILAN BIR ACTIONSCRIPT 3.0 KODUDUR */

// 1. EVENT DINLEYICI (Tarayiciya/Flash Playere Tiklama-Dinleme Kulakligi Tak)
// Stage(Global Oyun Sahnesi), Klavyeden Bir Tuşa (KEY_DOWN) basildiysa -> OyuncuyuHareketEttir Fonksiyonunu tetikle!
stage.addEventListener(KeyboardEvent.KEY_DOWN, OyuncuyuHareketEttir);


// 2. FONKSIYON VE OYUNCU MANTIIGI
// Bu fonksiyon "event (Eylem/Tus Olayi)" diye bir Arguman Alicak 
// (Dikkat: Void diyerek Geri Deger Donmeyen(Static C#) Kalitede bir JS Klonuydu AS3!)
function OyuncuyuHareketEttir(event:KeyboardEvent):void {
    
    // Eger Basilann Tus (Eventin Icindeki Tusa Basma Kodu == Klavyedeki SAG(RIGHT) Ok Tusu İse):
    if (event.keyCode == Keyboard.RIGHT) {
        
        // Ekranda (Sahne Hiyerarşisinde) kendi çizdiğim ÇöpAdam'ın Ismini (MC_CopAdam) cagrip
        // Onun yataydaki Koordinatini (x eksenini) 10 Piksel Ileriye (Saga) Sürükle Gitsin! 
        MC_CopAdam.x += 10;
        
        // Animasyon Modeli Cati_katında ise Yurume Kosümüne Gecsin (2. Frameye Git ve Oynat)
        MC_CopAdam.gotoAndPlay(2); 

    // Eger Kullanici BOŞLUK (SPACE) Tusuna Bastiysa?
    } else if (event.keyCode == Keyboard.SPACE) {
        
        // Cöp Adamin Y Kordinatini Cokert (Ziplatsin)
        MC_CopAdam.y -= 50; 
        
        // Ates etme Sesi (Kutuphaneye cektigmiz MP3 nesnesi) Cal(Play)!
        var tufekSesim:Sound = new TufekSesi(); // OOP Mucizesi : Yeni nesne uret
        tufekSesim.play();

    }
}
```
2005 yılında Bir Ergen, sadece bu Yukarıdaki Daktiloyla Trilyon Tıklanma Alan Flash oyunlar yazarak Miniclip veya Kongregate'de Para kazanabiliyor (AdSense zengini oluyordu). 

## Neden Öldü? Milyar Dolarlık İmparatorluğun Çöküşü 
1. **Güvenlik Kabusu:** Flash kodu çok Kolayca (Tersine-Mühendislikle/Decompile) Kırılabildiği ve Tarayıcıya virüs indirttiği için Antivirüs firmaları isyandaydı.
2. **2010 Steve Jobs Deklarasyonu "Thoughts On Flash":** Apple'ın CEO'su Steve Jobs, "Flash; Çok Pil/Şarj harcıyor, mobil telefonları kasıyor, Dokunmatik (Touch) için değil Fare (Mouse/Tıklama) için tasarlandı ve BUG'LI. BİZ YENİ cıkan iPHONE (DÜNYA DEVRİMİNE) FLASH PLAYER'I BİLEREK YÜKLEMEYECEĞİZ!" Şeklinde devasa o açık mektubu yayınladı. Dünyada Satılan Milyarlarca Cep Telefonunda Flash Oyunları Açılmayınca, Sistem felç oldu.
3. **HTML5, JS ve CSS3 'ün Gelecekte Doğuşu:** Youtube HTML5 in ( `<video>` ) Tagıyla Oynatıcı yapmayı , JS WebGL ile Frame-Oyun yapmayı başarınca, kimsenin Kapatılmış/Özel ve tehlikeli ActionScript kutusuna Tahammülü kalmadı. 2020 Yılında Chrome ve Microsoft Onu Windows Üzerinden Güvenlik Yamasıyla Tamamen Ölüm Fişini çekerek Yasakladı ve Gömüldü. Flash, dijital dönemin En İhtişamlı Şehididir.
