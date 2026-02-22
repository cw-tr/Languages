# WebGL

## Özet
WebGL (Web Graphics Library); 2011 yılında Mozilla Mühendisi Vladimir Vukićević'in öncülüğünde (Khronos Group tarafından) standartlaştırılan, **OpenGL ES'in (Mobil OpenGL'in) JavaScript diline Uyarlanarak İNTERNET TARAYICISININ İÇİNE Gömülmüş halidir.** Bilgisayara hiçbir uygulama/EXE yüklemeden, Sadece Google Chrome'dan web sitesine girerek Devasa Kalitede 3 Boyutlu Cisimleri ve Oyunları GPU (Ekran Kartı) İvmesiyle oynatabilmeyi sağlayan Web Devrimidir.

## Nedir ve Ne İşe Yarar?
1990'larda ve 2000'lerde Tarayıcı (İnternet sayfası) metin ve sıkıcı HTML-CSS resimlerinden ibaretti. İnternette oyun oynamak veya video izlemek için "Adobe Flash Player" eklentisini (Plugin) bilgisayara indirip kurmanız zorunluydu.

2011'lerde Mozilla Dedi ki: "Neden tarayıcımızın KENDİ İÇİNDE Yerel bir 3D çizim yeteneği yok? OpenGL bilgisayarın ekran kartına Zaten ulaşıyor, Biz OpenGL kodlarının birebir aynısını Javascript Fonksiyonları olarak Yazılımcılara Sunalım!".
WebGL ile Javascript üzerinden Tarayıcıdaki `<canvas>` HTML etiketinin içine doğrudan **Donanım Destekli 3D Render** alma Sihri doğdu!

**Ne İşe Yarar?**
* **Tarayıcı Tabanlı 3D Oyunlar ve Simülasyonlar:** Herhangi bir İndirme olmadan (Browser Games) çalışan, araba sürme oyunlari, interaktif 3D müze gezileri, Ayakkabı Satış Sitelerindeki "Ayakkabıyı 360 Derece Döndür" eklentilerinin Mimar Sinanı WebGL'dir.
* **Google Maps ve Earth:** Google Haritalarda Binaları, Dağları ve Gölgeleri 3D boyutlu olarak farenizle Çevirdiğinizde o görüntü arkaplanda Saf WebGL kullanılarak Ekran Kartınız tarafından internet sitesinde çizdirilmektedir!

## Dilin Mantığı ve Kod Yapısı
Dili; %100 oranında **JavaScript (Veya TypeScript)**'tir. 
Yazılım Akışı (Mantığı) ise tamamen **OpenGL ES 2.0 (Ve 3.0)** C++ mantığının Saf Birebir JavaScript'e Çevrilmiş (Çakma C++) Kopyasıdır. (State Machine Mimarisi).

Ancak WebGL ÇÇOOOOK Zor, uzun ve Amele(Boilerplate) bir sintaksa sahiptir. Siyah Rengi ayarlamak için bile 50 satır Güvenlik tamponu(Buffer) Javascripti yazmanız Gerekir.

Bu yüzden Geliştiricilerin %99'u Asla SIFIRDAN WebGL yazmaz! Bunun yerine WebGL'in karmaşıklığını "İnsancıl OOP/Sınıflara" çeviren meşhur **Three.js** Veya **Babylon.js** gibi Kütüphaneler/Oyun Motorları kullanırlar.

**Örnek İşleyiş (JavaScript'teki Vahşi WebGL):**
Html: `<canvas id="oyunAlanim"></canvas>`
JS: `const gl = document.getElementById('oyunAlanim').getContext('webgl'); gl.clearColor(0.0, 0.0, 0.0, 1.0); gl.clear(gl.COLOR_BUFFER_BIT);`

### Örnek Bir WebGL Kodu: Kütüphane (Three.js) Kullanarak Tarayıcıda Dönen Bir Küp Yapmak
Aşağıda "Boş(Vanilla)" WebGL (500 satır sürer) yerine; Milyonların WebGL'i kullanmak için sarıldığı Three.js Kütüphanesi ile yazılmış modern bir Browser 3D Render'ı Şöyledir:

```javascript
// THREE.JS (WebGL'i kolaylaştıran Dünya Lideri Kütüphane) kullanarak Web Tarayicisinde DÜnya Olustyrmmak:

// 1. SAHNE (WORLD) VE KAMERA (GOZ)
// 3 Boyutlu Karanlik uzayi (Sahne) yaratiyoruz
const scene = new THREE.Scene();

// Bir Kamera Yaktiyoruz (Gorus_Acisi(75), Ekran_Orani, Piksellerin_Yakisi_ve_Uzak_Limiti)
const camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );
// Kamerayi uzayda geriye Cek! (Cunku kup tam x=0'da Cikacak, 0'da olursak icinde kaliriz)
camera.position.z = 5;


// 2. RENDERER (WEBGL FIRÇASI - EKRAN KARTINA BAGLANAN MOTOR)
const renderer = new THREE.WebGLRenderer();
// Fircanin boyunu Ekrannin Tamami yap
renderer.setSize( window.innerWidth, window.innerHeight );
// HTML Bedenine (Body'e) bu Çizim Tahtasini (Canvas) Mıhla/Ekle!
document.body.appendChild( renderer.domElement );


// 3. 3 BOYUTLU MODEL (GEOMETRI) YARATIMI: 
// Matematiksel bir KUP (1x1x1) Iskeleti(Geometrisi) yarat:
const geometry = new THREE.BoxGeometry( 1, 1, 1 );

// Kupe Kaplama / Materyal (Boya) Ayarla: Metalik/Plastik Yesil Renk
const material = new THREE.MeshBasicMaterial( { color: 0x00ff00 } );

// Iskelet ile Boyayi Birlestir -> Fiziksel KUP OLUSTU (MESH)
const cube = new THREE.Mesh( geometry, material );

// Kupu Karanlik Sahneye Gonder:
scene.add( cube );


// 4. ANIMASYON DONGUSU (OYUN LOPPU) 
// Tarayici Saniyede 60 Kare dondurdukce (Frame gectikce) kupun Acsini yamult!
function animate() {
    
    // Tarayiciya "Sıradaki animasyon Faresinde(Karesinde) beni uyar/Tekrar cagir" de (Loop):
	requestAnimationFrame( animate );

    // Kupe saniyede Ufak rotasyonlar(Donmeler) ver: X ve Y eksenin etrafinda
	cube.rotation.x += 0.01;
	cube.rotation.y += 0.01;

    // EKRAN KARTIYLA BIRLIKTE SAHNEYI KAMERADA GORUNDUGU SEKILDE CIZ!! (Asil WEBGL Cirtmasi)
	renderer.render( scene, camera );
}

// Oyunu Baslat!
animate();
```
İşte sadece bu kısacık Javascript Kodunu bir HTML'in İçine gömdüğünüz anda; Web Sitesinde (Hiçbir eklenti gerekmeden) Havada kendi etrafında Saniyede 60-FPS Işık hızında pürüzsüzce dönen Parlak Yeşil bir 3 Boyutlu Kutu peydahlanır. Web'in Gücüdür.

## Kimler Kullanır?
* Kendi Porföyüne giren Müşterileri Havalı 3D Web Menüleriyle veya "İnteraktif Ürün Sergileriyle" etkilemek isteyen **Yaratıcı Ön-Yüz (Creative Front-End) Geliştiricileri**.
* Kripto Varlık Platformları/Metaverse oyun (Axie Infinity vb) harabesinde Gezilen evrenleri Web Tarayıcısı açan Browser-Developer mimarları.
* Khronos Group Günümüzde Daha iyi perfomans ve çok çekirdek vadeden (Vulkan / Metal tabanlı) yeni **WebGPU** standardını Çıkartmış olsa da; WebGL milyonlarca Mimaride Web'in Tek 3D Patronu/Diktatörüdür.
