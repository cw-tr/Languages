# Node.js

## Özet
Node.js; 2009 yılında Ryan Dahl tarafından icat edilen, normal şartlarda **SADECE** İnternet Tarayıcısının (Örn: Google Chrome) kalbinde yaşayan JavaScript dilini; Tarayıcıdan koparıp (V8 Engine'i Masaüstüne C++ ile derleyerek) doğrudan **Bilgisayarın Arkasında/İşletim Sisteminde (Sunucu/Backend)** çalıştırmayı başaran Devrimsel bir "Çalıştırma Ortamı (Runtime Environment - Çerçeve de Değildir, Bir Makinedir)" teknolojisidir.

## Nedir ve Ne İşe Yarar?
2009'a kadar kural şuydu: Sadece kullanıcı tarafında(Front-End) HTML tıklamalarında JavaScript yazılırdı. Arka planda (Backend) yani Veritabanına bağlanan, C diskinden Word dosyası okuyan, Apache sunucusunda ağ(TCP) isteği dinleyen diller Java, PHP, C# veya Python'du. (Tarayıcı güvenlik sebebiyle C diskini Okuyamazdı, yani JS de okuyamazdı).

Ryan Dahl, Google Chrome'un efsanevi V8 Engine (Javascript Metnini C makine koduna anında çeviren motor) kütüphanesini Chrome'un kaynak kodundan bıçakla Cımbızlayıp(Kesip), masaüstü bir C++ programı içine gömdü ve buna Node adını verdi. Artık "Konsola" (CMD) `node app.js` yazdığınızda JavaScript; Tarayıcı olmadan Bilgisayarda bir PHP, bir Python gibi koşturuyor, C diskinde Dosya Siliyor ve SQL Veritabanında fırtınalara sebep oluyordu!

**Ne İşe Yarar?**
* **Tam İstif (Full-Stack) Geliştirme Efsanesi:** Önceden Ön-Yüz(FrontEnd) geliştiricisi (JS Bilen Adam), arka plana(Backend) geçeceğinde mecburen PHP veya C# öğrenmek zorundaydı. Node.js sayesinde bir kişi SADECE Javascript(/Typescript) öğrenerek Web Sitesinin başından sonuna, Sunucu Veritabanından Tarayıcı UI'a kadar (Uçtan uça) Her Şeyi (Full Stack) aynı ekosistem içinde üretebilir hale geldi.
* **Gecikmesiz Canlı İletişim (WebSocket / Chat Sunucuları):** Node.js Olay-Güdümlü (Event-Driven) ve Non-Blocking (Bloke Etmeyen / Tek İş Parçacıklı Async) muazzam bir C++ mimarisi (Libuv) ile gelir. Milyonlarca kişinin aynı anda bağlı olup Canlı Chatleştiği (WhatsApp Web, Discord Sunucuları, Multiplayer Tarayıcı Oyunları) soketleri Apache(PHP)'nin çöküntüsünden kurtaran ve aynı anda binlerce kullanıcıyı Işık hızında yollayan eşzamansız (Async) dev bir Trafik Bekçisidir.

## Dilin Mantığı ve Kod Yapısı
Dili (Yeni ek bir dil üretmemiştir) doğrudan JavaScript'tir. Ancak Tarayıcılarda asla bulunmayan "Backend/İşletim Sistemi" kütüphanelerini C++ Cekirdeklerinde tutar (Örn: `fs` FileSystem, `http`, `crypto`, `os`). 

Tarayıcıda olmayan `require()` (Ortama Paket Yükleme) modülüyle, dünyadaki en BÜYÜK ve devasa kod kütüphanesi olan **"NPM" (Node Package Manager)** ekosistemine sahiptir. Bilgisayarınızda (CMD) `npm install express` yazıp saniyede Web Sunucusu çerçevesine (API) sahip olursunuz.

**Örnek İşleyiş:**
Normal JS (Tarayıcı): `document.getElementById('div');`
NodeJS (Terminal JS): `const fs = require('fs'); fs.writeFileSync('belge.txt', 'Hacklendin!');`

### Örnek Bir Node.js Kodu: 10 Satırla Dünya Çapında Bir HTTP Web-Server (API) Açmak!
Eskiden Apache sunucuları, PHP Kurulumları XAMPP panelleri kovalayıp C++ ile port dinleme yazılan karanlık kaba-kuvveti; 2 gram JavaScript ile "Yerel Bilgisayarda Milyonlarca İsteğe açık (Localhost:3000) REST-API" ye dönüştüren devrim:

```javascript
/* BU KOD NODEJS ILE KONSOLDAN CALISIR TARYICADA CALISMAZ (Backend!) */

// 1. DAHILI KUTUPHANE CEKIMI (C/PHP Include mantigi)
// Isletim sisteminin(Node) "http" ag portlarini dinleyen Ana kartini Koda bagla:
const http = require('http');


// 2. SERVER (SUNUCU) MUCUZESISI (Callback Hell / Event Dinleme)
// CreateServer fonksiyonu, dunyanin herhangi bir yerinden Senin IP-Adresine Giren Biri olduugunda "Tetiklenir" ve su ok(=>) fonskiyonunu Cagirir!
const sunucu = http.createServer((istek_client, cevap_server) => {
    
    // Biri Sitemize Baglandi! Tarayiciya(Bize baglanan kisiye) 200(OK/BAŞARILI) Kodu firlat ve Icerik Tiipinin HTML oldugunu soyle:
    cevap_server.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
    
    // Adamın Tarayacisina(Ekrana) "Arka taraftan - Veritabanindan" String bas(Yolla!):
    cevap_server.write("<h1>🚀 Selam Dunya! Node.js Arka-Plan Sisteminden Selam.</h1>");
    cevap_server.write("<p>Artik C# veya PHP'ye muhtac degilziniz. JS Her Yere Hukmetti.</p>");
    
    // Baglantiyi kapat ve Musteriye(Clienta) HTML Dokumanini paketleyip Kargo et:
    cevap_server.end();

});


// 3. PORTU (KAPIYI) ACIP DINLEMEYE BASLA
const CIKIS_PORTU = 3000;

sunucu.listen(CIKIS_PORTU, () => {
    // Bu Cikiti sadece Bizim (Windows DOS Terminalimizde/Konsolumuzda) arka planda Gozukur
    console.log(`Sunucu Ayaga Kalkti! HTTP istegi su adresten bekleniyor: http://localhost:${CIKIS_PORTU}`);
});
```

Bu belgeyi `server.js` olarak kaydedip Komut İstemcisine (Terminal) çıkıp "node server.js" yazıldığı saniye, bilgisayarınız evrensel bir (Apache eşdeğeri) İnternet Sunucusuna dönüşür. Milyonlarca Javascriptçi bu güçle tanışınca 2012 lerde Full-Stack rüzgarı tüm interneti kasıp kavurdu. (Node üstünde genelde "Express.js" framework'ü kullanılır). 

## Kimler Kullanır?
* Evrendeki modern **Full-Stack / Backend Developer (Node.js/Express) Geliştiricileri**.
* Özellikle Netflix, Uber, PayPal gibi devasa kurumsal şirketler saniyedeki Milyarlarca Kullanıcı Ağ-(I/O Data) Requestini ve Canlı veri akışını (Streaming) idare ederken Çökmemek / RAM Tıkamamak için Java'dan bile Node'a (Kısmen MicroServisler için) Göçmüşlerdir.
* IoT (Akıllı Cihaz) backend otomasyonlarında yüngülük (Lightweight) için kurulur.
* Günümüzde Node.js'e rakip olarak (yine aynı kafadan ve kendi yaratıcısı Ryan Dahl'dan çıkan) daha güvenli **Deno** ve C++'tan çok Zig+Rust karışımı ışık hızındaki **Bun** runtime'ı çıksa da, NPM pazarının Devasa tekel kütüphanesi nedeniyle Node uzun yıllar Backendin Krallarındandır.
