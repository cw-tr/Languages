# GLSL (OpenGL Shading Language)

## Özet
GLSL (OpenGL Shading Language); 2004 yılında Khronos Grubu ve 3DLab firması (OpenGL konsorsiyumu mimarları) tarafından icat edilen, yazılımın **Ekran Kartınızın (GPU) yüzbinlerce çekirdeğinde** aynı anda "Piksel Boyamak" (Shader / Gölgeleyici) olarak koşmasını sağlayan ve doğrudan grafik donanımı ile konuşan, C kökenli spesifik bir **Görsel/Fizik Motoru (Grafik Boru Hattı)** dilidir.

## Nedir ve Ne İşe Yarar?
Eskiden (90'lı yıllarda) 3 Boyutlu (3D) bir oyunda arabanın gölgesinin duvara yansıması veya suyun (okyanusun) dalgalanma ışığı gibi grafik operasyonları "Ekran Kartının içine Lehimlenmiş (Sabit) bir kural (Fixed Pipeline)" ile gelirdi. Programcılar ("Ben okyanusu mor yansıtmak, arabanın demirini çizikli parlatmak istiyorum") dediğinde hiçbir esneklik yoktu.

OpenGL Mimarları, C dilinden (Syntax olarak) ilham aldılar ancak "Bu kodu Normal İşlemcide (CPU'da) tek sıra değil; Ekran Kartının (GPU) 10.000 Çekirdeğinde paralel C dili kodları gibi Eşzamanlı Derletelim" dediler. Ortaya Shader/GLSL çıktı. Artık yazılımcılar; Suyun kırılma açısını (Fragment) VEYA 3 Boyutlu Köşelerin Koordinatlarını (Vertex) kendi C-Komutlarıyla ekranda piksellere basabiliyordu!

**Ne İşe Yarar?**
* **3D Oyun Motorları (Unity, Unreal, Custom C++ Oyunlar):** Oynadığınız o mükemmel grafikli bilgisayar oyunlarındaki "Yanan Alev efektleri, Okyanus Köpükleri, veya Mat/Metal Parlama gölgelendirmeleri" %100 Oranda ekran kartının içinde bu dille (GLSL veya DX HLSL ile) derlenir / yırtılır. (Oyun motoru arkaplanda GLSL Shader atar).
* **Web Tarayıcısında 3 Boyut Devrimi (WebGL):** (Three.js isimli kütüphanenin kalbindeki ruh!). Google Chrome'da izlediğiniz 3D Kılıç Sallama (Browser tabanlı) oyunların tek donanım mimarisidir; Çünkü WebGL doğrudan GLSL (String formatında kodları) alır ve Telefon/Android GPU'suna basar! Shader Toy sanatkarları bu dili kullanır.

## Dilin Mantığı ve Kod Yapısı
Tamamen (Tıpatıp) **C Diline benzer**. Ama String, veya Class, Array (Liste) eziyetleri bilmez. En yüce gücü "Matematiksel Vektörler (`vec2`, `vec3`, `vec4`) ve Matrisleri (Matrixler)" havada tek satırda Çarparak ekrandaki IŞIK yansımasını milisaniyede hesaplama algoritmasıdır.

Program ÇEKİRDEKTEN ikiye ayrılır (Ayrı ayrı derlenen iki Dosyadır/Programdır):
1. **Vertex Shader (Köşe Parçalayıcı):** Elindeki 3 Boyutlu (Kutu vb) Modelin, X-Y-Z Noktalarını (Köşelerini) hesaplayıp, Kameranın(Adamın Gozlerinin) baktığı 2D Ekrana doğru ezip(Perspective/Projection Math) sığdırmak.
2. **Fragment Shader (Piksel Boyayıcı):** Yukardaki köşelerin İÇİNİ, Tek tek piksellerini dolaşarak "Bana kırmızıyı maviye vur" diyerek Rengini/Aydınlatmasını bulmak/boyamak (Her piksel için aynı anda paralel koşar!).

**Örnek İşleyiş (Sembolik Olarak):**
Ana C/Java Motoru ekran kartına `draw()` komutu yollar (Milyonlarca vertex).
GPU'daki `Fragment Shader (GLSL)` o noktalara tek tek ışık matematiği (`dot(parilti, isikAcisi)`) yollar! Düşükse siyaha, yüksekse Bembeyaz (Parlama) vurur.

### Örnek Bir GLSL (Fragment Shader) Kodu: Ekranda Sürekli Dalgalanan/Dönen Renkler (Gökkuşağı Shader'ı Gösterimi)!
Oyunların sularındaki o animasyonlu parlamanın ta kendisi (Her bir pikselde bağımsız çalışan Vector(Renk Red-Green-Blue-Alpha[RGBA]) Hesaplayıcı Işık zekası!

```glsl
// GLSL Dili klasik C // veya /* */ Yorum Satirlarini Kullanir.
// Ozel ve Çok Güçlü Bir Fragment(Piksel-Renk) Kodudur. Ekran kartindaki HER TANE PİKSEL onu calistirir!

// "Precision" : GPU'ya Matematik Kusuru Ver! 
//(Telefonda hizli calissin diye Float(ondaliklilar) orta-kalitede(mediump) tutulsun):
precision mediump float;

// 1. DISARIDAN GPU'YA GELEN ENJEKSİYONLAR (Uniforms)
// "Uygulamanın" (ornegin Python/JS Oyun Motorunun GPU'ya 'Suanki Zaman Kac' diye fisildadigi) Degisken.
// Her piksel icin bu (zaman) ortaktir:
uniform float uZamanAkisi; 

// Ekranda (X, Y) olarak şu an tam olarak hangi Kordinattaki Pikseli boyuyoruz bilgisı: (Input)
varying vec2 vDokunmaKordinati; 

// 2. MAIN(KONTRAKT FOKSIYON): Ekrandaki Trilyon Pikselin Rengine ne puskurteyim?
void main() {
    
    // Uzerinde Gezindigim Piksel(Vektor Noktam): Sifir(Sol) ile Bir(Sag/Ust) arasindadir (Uv kordinati)
    // Suna (vDokunmaKordinati) diyelm X:0.5 , Y:0.5 (Yani Ekranin Tam Ortasini ciziyor! GPU!)
    vec2 pos = vDokunmaKordinati;
    
    // MUCİZE SİNUS MATEMATİGİ: (Dalgalanma Ruju)
    // Pikselin Kırmızı Rengini (R) Zamana gore Dalgalandır (-1 ile +1 arasinda Doner)
    float R_KırmızıGucu = sin(pos.x * 10.0 + uZamanAkisi);
    
    // Pikselin Mavi ve Yeşil rengini baska kordinatlarla karıştır
    float G_YesilGucu   = sin(pos.y * 10.0 - uZamanAkisi);
    float B_MaviGucu    = 0.5; // Sabit donuk mavi rengi ver.
    
    /* IŞIK ve KONTRAST AYARI: (Sinus Eksi de cikabilir, -1 ile 1 arasini -> Gorsel Renkleri Olan 0.0 - 1.0 araligina normalize et ki Siyah patlamasi olmasin!)
       Rakamı 0.5 ile topla ve ikiye bol! C/C++ matematiksel matris gucu!
    */
    R_KırmızıGucu = R_KırmızıGucu * 0.5 + 0.5;
    G_YesilGucu   = G_YesilGucu * 0.5 + 0.5;
    
    
    // GL_FRAGCOLOR: SIHIRLI SÖZCÜK! BU PİKSELE KANA BOYA (Vector4 -> R, G, B, Alfa/Gorunurluk Saydamligi)
    // Alfa (A)= 1.0 (Yani tam kati boya(Saydam degil)).
    gl_FragColor = vec4(R_KırmızıGucu, G_YesilGucu, B_MaviGucu, 1.0);
}

// BU KOD; MILYONLARCA CUDA(GPU) Cekirgeginde Ayni anda isleyip Ekranda rengarenk donen disko isikli Okyanuslar (Ripple) yansitmasi acar.
```
C ve C++ geliştiricisi, "Kılıç" kutusunun modelini diske yazar, oyun oynanırken GPU bu "C-Kodu formundaki Şeytanı" Derler atar; ve O Kılıç kutusunun üzerinde Güneş Parlaması / Kana Bulama Vektör Matris operasyonları harikulade pürüzsüz animasyonlarla hesaplanır.

## Kimler Kullanır?
* Evrendeki bütün **C++ / Oyun Grafik Donanım Mühendisleri (Graphics Programmers)**. Oyun motorlarına Işık Hüzmesi / Motion Blur (Hareket bulanıklığı) vb Custom FX yamanlar.
* **Three.js , WebGL veya WebGPU Web Tasarım/Animatörleri / Creative Coders:** "Shadertoy.com" üzerinde sadece Yukarıdaki gibi matematik/sinüs formüllerini dizip Akıl almaz Kara Delik Simülasyonları tasarlayan Sanat/Matematik Uzmanları (Creative Coder Felsefecileri). C, Javascript, Python projelerine tutkal komut/metin olarak gömüldüğü için bağımsız çalıştırılamaz "GPU'nun Eklentisidir".
