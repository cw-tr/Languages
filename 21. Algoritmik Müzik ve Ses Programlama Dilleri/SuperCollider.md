# SuperCollider

## Özet
SuperCollider; 1996 yılında James McCartney tarafından icat edilen, hem gerçek zamanlı (Real-Time) bir Ses Sentezleme Motoruna (scsynth) hem de bu motoru kontrol edip algoritmak müzik (Algorithmic Composition) yazmanızı sağlayan nesne yönelimli dinamik bir programlama diline (sclang) sahip olan, dünyadaki en saygın, en derin ve en akademik **Elektronik Müzik, Ses Tasarımı ve Canlı Performans (Live Coding) Dilidir.**

## Nedir ve Ne İşe Yarar?
Eğer bir müzisyenseniz ve "Gitar" veya "Bateri" sesi Çıkarmak istiyorsanız FL Studio (veya Ableton) programını Açar, Hazır Kayıtlı (Sample) veya Sanal enstrüman (VST) sesini farenizle nota(MIDI) dizerek Çalarsınız.

Fakat siz bir "Ses Mühendisi (Sound Designer) Veya Akademisyen" iseniz, Fareyle nota basmak istemezsiinz.  **"Saniyenin onda biri Hızında, Frekansı Sinüs Dalgasına Vuran, O Sinüs Dalgasını alıp Çan Eğrisinden Geçiren ve Matematiksel olarak Evrende Hiç Duyulmamış (100% Sentetik) Bir Ses Dalgası"** Üretmek istersiniz! 

SuperCollider Der ki: Fareyi ve Klavyeyi(Midi) At! Bana C++ tarzı nesneye yönelik Kod Yaz! Ekranda `SinOsc.ar(440)` yazıp ENTER (Shift+Enter) tuşuna bastığın SANİYE Gecikmesiz olarak Hoparlörden O Sinüs dalgasını(Nota La) çalmaya Başlayacağım! 

**Ne İşe Yarar?**
* **Canlı Kodlama (Live Coding / Algorave):** Londra'da veya Berlin'de Gece Kulüplerinde DJ'lerin Arkada "Play" tuşuna basmadığı, Konser Sırasında Terminal (Siyah Ekran) Açıp Gözü Dönmüş gibi KOD YAZARAK Dinleyicilere Techno ve Trance Müzik Cılgınlıkları yasattığı O Mükemmel Sanat Akımının Kalbigidg. Devam Eden Sesi Kapatmadan Algoritmayı editleyip Yeni vuruş (Beat/Kick) atarsınız!
* **Akustik Araştırma:** Üniversitelerin İşitme, Dalga Matematiği(Fourier Transforms)  Ve Müzikolojği Bölmümlerindeki Professörle, İnsan kualginn duyabbilecegii En spesikk Hzelri Urettmek Iucujn Kullanriıalr. 

## Dilin Mantığı ve Kod Yapısı
Sistem 2 Parcadan Olurur: 
1. **Sclang (SuperCollider Language):** Smalltalk diline dayanan, fonksiyonel özellikleri olan Müzisyenin/Programcının Yazdıği Dildir.
2. **Scsynth (Motor/Sunucu):** O yazdiijgın Kodari Alip O Saniyede (C++) Ses Kartina İşleyip Hoparlore Vuran Saf Ses Çekirdeği!

Yazım Şeklisi (Syntax) Parçalara Odamlıdır(Ugens - Unit Generators). Dalgra Yaratır, Filtreden Geçirir, SSe(Out) Verirsisz.

### Örnek Bir SuperCollider Kodu: Matematikle ve Kodla "Canlı Synthesizer (Elektronik Müzik Aleti)" Yaratmak!!!
Sadeece Ektana Kod Yazip, Kodu Sectiten sonra **"CTRL+ENTER"** Tusuna Badikginzda Hopralorden Odayi Titetectek(Ve Asla Duramayacvak) Temeril Elektronki Sessei Ürettne Kod:

```supercollider
// BU BIR SUPERCOLLIDER (.scd) KODUDUR

// 1. SES MOTORUNU (SUNUCUYU) BASLAT
s.boot;  // (Saniyede C++ Ses Cekirdegi Ayaga Kalkar ve Kulakliklara Baglanir)


// 2. IŞIK HIZINDAKI CANLI KOD: MATEMATIKSEL KEMAN / SYNTHESIZER CALMAK!
// Asagidaki Blogu(Parantnzler arasini) Secip CTRL+ENTER Yaptgnidzda Muizkk BRsalar!
(
{
    // A) TEMEL DALGA (Ses Teli): 
    // SinOsc (Sinüs Oslatoru/Dalgsy), 440 Hz (Nota: Do / La), Ar (Audio Rate - Ses hizinda Calis)
    var ses_dalgasi = SinOsc.ar(freq: 440, mul: 0.5); 
    
    // B) LFO (Low Frequency Oscillator) - TİTREŞİMM (Vibrato) VERMK!
    // Sesi tekduze degillde, Sanyiede 4 Kere İndi Cıkti(Titreetn) Bir Efektre sok!
    var titreme = SinOsc.kr(4); // Kr = Control Rate (Yavass Calisip Sesi ezeer)
    ses_dalgasi = ses_dalgasi * titreme;

    // C) FILTRELEME / EFEKT: Pürüşlügünü Alpp Revereb(Yanki) Ver!
    // LPF (Low Pass Filter), FreeVerb (Aciks alan Yaniksiisi)
    var yumusak_ses = LPF.ar(in: ses_dalgasi, freq: 1000); // 1000 Hz usutunu Kes!
    var yanki_verilmsi = FreeVerb.ar(in: yumusak_ses, mix: 0.5, room: 0.9);

    // D) CIKTIII YUUUU VER! 
    // Out.ar (Oudio Outupt). 0(Sol Kulakcik), yanki_verlmsi(Sag Kuallakcik Stereo Yaptiki)
    Out.ar(0, [yanki_verilmsi, yanki_verilmsi]);
    
}.play; // .play Emri => Yukaridaki C++, Yapisin Anidaa Hopaarloere SUTA demketiir!
)
```

Bu kodu çalışıtrgınızda, Mause ile (Abletin gibi) hicvbr Çİzim yapmadan Oda İçeridne "Güm, güm" giden Mükelmel Bir Techno Sesinin yankılarılula Duyarasnız. Yzkardaki `freq: 440` Yazarn Yeri Sİlİp `800` yazip CTRLEnter Bastiginjzda, **Önceki Ses Kesilmeden Müzik Canli CaMLİ ÜÜSte DOgru Evrilmeye((Live Cding)** BAaşklr!

## Kimler Kullanır?
* Evrendeki Neredeye Büytün "Bağımsız (Avant-Garde)" Elektronik Şarkıcılar (Örn: Aphex Twin gibi ismileriin de Koddaki Sentzelerindee Bu Yatar).
* Sanat (Algorave) Etkinliklerinde Bilgisaysarın Başına Çıkıp Bize Ekrannu Yansitiraeek **Terminladne Müzükl Şöleni Vren Live-Coder'lar**. Ve Sinema sektöündrki Dev Sci-Fi(Bİilim Kurgu Uzay) Filmlerine Efket Üreten Ses Müeendisterkudre. (Yazlıılmcunın, Sanatıa En Yaklaştıği UçNokdratdıdır).
