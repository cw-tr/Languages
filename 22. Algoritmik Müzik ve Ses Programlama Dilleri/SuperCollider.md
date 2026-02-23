# SuperCollider

## Özet
SuperCollider; 1996 yılında James McCartney tarafından icat edilen, hem gerçek zamanlı (Real-Time) bir Ses Sentezleme Motoruna (scsynth) hem de bu motoru kontrol edip algoritmik müzik (Algorithmic Composition) yazmanızı sağlayan nesne yönelimli dinamik bir programlama diline (sclang) sahip olan, dünyadaki en saygın, en derin ve en akademik **Elektronik Müzik, Ses Tasarımı ve Canlı Performans (Live Coding) Dilidir.**

## Nedir ve Ne İşe Yarar?
Eğer bir müzisyenseniz ve "Gitar" veya "Bateri" sesi Çıkarmak istiyorsanız FL Studio (veya Ableton) programını Açar, Hazır Kayıtlı (Sample) veya Sanal enstrüman (VST) sesini farenizle nota(MIDI) dizerek Çalarsınız.

Fakat siz bir "Ses Mühendisi (Sound Designer) Veya Akademisyen" iseniz, Fareyle nota basmak istemezsiniz. **"Saniyenin onda biri Hızında, Frekansı Sinüs Dalgasına Vuran, O Sinüs Dalgasını alıp Çan Eğrisinden Geçiren ve Matematiksel olarak Evrende Hiç Duyulmamış (100% Sentetik) Bir Ses Dalgası"** Üretmek istersiniz! 

SuperCollider Der ki: Fareyi ve Klavyeyi(Midi) At! Bana C++ tarzı nesneye yönelik Kod Yaz! Ekranda `SinOsc.ar(440)` yazıp ENTER (Shift+Enter) tuşuna bastığın SANİYE Gecikmesiz olarak Hoparlörden O Sinüs dalgasını(Nota La) çalmaya Başlayacağım! 

**Ne İşe Yarar?**
* **Canlı Kodlama (Live Coding / Algorave):** Londra'da veya Berlin'de Gece Kulüplerinde DJ'lerin Arkada "Play" tuşuna basmadığı, Konser Sırasında Terminal (Siyah Ekran) Açıp Gözü Dönmüş gibi KOD YAZARAK Dinleyicilere Techno ve Trance Müzik Çılgınlıkları Yaşattığı O Mükemmel Sanat Akımının Kalbidir. Devam Eden Sesi Kapatmadan Algoritmayı editleyip Yeni vuruş (Beat/Kick) atarsınız!
* **Akustik Araştırma:** Üniversitelerin İşitme, Dalga Matematiği (Fourier Transforms) Ve Müzikoloji Bölümlerindeki Profesörler, İnsan Kulağının Duyabileceği En Spesifik Sesleri Üretmek İçin Kullanırlar. 

## Dilin Mantığı ve Kod Yapısı
Sistem 2 Parçadan Oluşur: 
1. **Sclang (SuperCollider Language):** Smalltalk diline dayanan, fonksiyonel özellikleri olan Müzisyenin/Programcının yazdığı Dildir.
2. **Scsynth (Motor/Sunucu):** O yazdığın Kodları Alıp O Saniyede (C++) Ses Kartına İşleyip Hoparlöre Vuran Saf Ses Çekirdeği!

Yazım Şekli (Syntax) Parçalara Odaklıdır (Ugens - Unit Generators). Dalga Yaratır, Filtreden Geçirir, Sese (Out) Verirsiniz.

### Örnek Bir SuperCollider Kodu: Matematikle ve Kodla "Canlı Synthesizer (Elektronik Müzik Aleti)" Yaratmak!!!
Sadece Ekrana Kod Yazıp, Kodu Seçtikten sonra **"CTRL+ENTER"** Tuşuna Bastığınızda Hoparlörden Odayı Titretecek (Ve Asla Durmayacak) Temel Elektronik Sesi Üreten Kod:

```supercollider
// BU BIR SUPERCOLLIDER (.scd) KODUDUR

// 1. SES MOTORUNU (SUNUCUYU) BASLAT
s.boot;  // (Saniyede C++ Ses Çekirdeği Ayağa Kalkar ve Kulaklıklara Bağlanır)


// 2. IŞIK HIZINDAKI CANLI KOD: MATEMATIKSEL KEMAN / SYNTHESIZER CALMAK!
// Asagidaki Bloğu (Parantezler arasını) Seçip CTRL+ENTER Yaptığınızda Müzik Başlar!
(
{
    // A) TEMEL DALGA (Ses Teli): 
    // SinOsc (Sinüs Osilatörü/Dalgası), 440 Hz (Nota: Do / La), Ar (Audio Rate - Ses hızında Çalışır)
    var ses_dalgasi = SinOsc.ar(freq: 440, mul: 0.5); 
    
    // B) LFO (Low Frequency Oscillator) - TİTREŞİM (Vibrato) VERMEK!
    // Sesi tekdüze değil de, Saniyede 4 Kere İnip Çıktı (Titreten) Bir Efekte sok!
    var titreme = SinOsc.kr(4); // Kr = Control Rate (Yavaş Çalışıp Sesi ezer)
    ses_dalgasi = ses_dalgasi * titreme;

    // C) FILTRELEME / EFEKT: Pürüzlülüğünü Alıp Reverb (Yankı) Ver!
    // LPF (Low Pass Filter), FreeVerb (Açık alan Yankısı)
    var yumusak_ses = LPF.ar(in: ses_dalgasi, freq: 1000); // 1000 Hz üstünü Kes!
    var yanki_verilmesi = FreeVerb.ar(in: yumusak_ses, mix: 0.5, room: 0.9);

    // D) ÇIKTIYI ŞU VER! 
    // Out.ar (Audio Output). 0 (Sol Kulakçık), yanki_verilmesi (Sağ Kulakçık Stereo Yaptık)
    Out.ar(0, [yanki_verilmesi, yanki_verilmesi]);
    
}.play; // .play Emri => Yukarıdaki C++ Yapısını Anında Hoparlöre ŞUTLA demektir!
)
```

Bu kodu çalıştırdığınızda, Mouse ile (Ableton gibi) hiçbir çizim yapmadan Oda İçerisinde "Güm, güm" giden Mükemmel Bir Techno Sesinin yankılarıyla duyarsınız. Yukarıdaki `freq: 440` yazan yeri silip `800` yazıp Ctrl+Enter bastığınızda, **Önceki Ses Kesilmeden Müzik Canlı CANLI Üste Doğru Evrilmeye ((Live Coding)** Başlar!

## Kimler Kullanır?
* Evrendeki Neredeyse Bütün "Bağımsız (Avant-Garde)" Elektronik Şarkıcılar (Örn: Aphex Twin gibi isimlerin de Sentezlerinde Bu Yatar).
* Sanat (Algorave) Etkinliklerinde Bilgisayarın Başına Çıkıp Bize Ekranı Yansıtarak **Terminalden Müzikal Şöleni Veren Live-Coder'lar**. Ve Sinema sektöründeki Dev Sci-Fi (Bilim Kurgu Uzay) Filmlerine Efekt Üreten Ses Mühendisleridir. (Yazılımcının, Sanata En Yaklaştığı Uç Noktasıdır).
