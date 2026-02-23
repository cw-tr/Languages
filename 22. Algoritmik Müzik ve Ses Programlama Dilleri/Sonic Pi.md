# Sonic Pi

## Özet
Sonic Pi; 2012 yılında Cambridge Üniversitesi'nden Dr. Sam Aaron tarafından (Aslen Raspberry Pi mikrobilgisayarlarında çocuklara yazılımı sevdirmek amacıyla) icat edilen; ancak zamanla devasa bir büyüme yaşayarak SuperCollider'ın karmaşıklığını "Ruby Dilinin Efsanevi Okunabilirliğiyle" harmanlayan, Dünyadaki en erişilebilir, eğlenceli ve güçlü **Eğitim Odaklı Canlı Performans (Live Coding / Algorave) Müzik Dilidir.**

## Nedir ve Ne İşe Yarar?
SuperCollider harika bir Dildi ama Sintaksı(Yazım Şşeklli) SmallTalk kökenli olduğu için İlkokul/Lİse veya Müzik-Yazılımına Yeni aşlayack İnsanları "C++ gibi Hatalatla" Korkutuyordu. (Paratnzler, Süslü Oklar, Sınıflarla Adam Boğuluyordu).

Sam Aaron Dediki: **Öyle bir Dil Yapacağım ki, Çocuklar sadece İngilizce 'Çal 60' (play 60) Yazacak ve Müzik Başalayacak! Ama O dili İsteyen Yetişkin BiR DJ Alıp, Arkada SuperCollider Motoruna Baglanarak Gecve Kulubüünü Sallaeyıabikecel!**
Dilin tabanını (Yazılım Arayüzünü) Okunabilirliğiyle Meşhur OLAN **Ruby** Diliyle İnşa etti, Ama "Sesi CıokartaCek" Olan Kalbini (Scsynth - SuperCollider Engine) olarak Ayarladı.  Ortayua Oynanmasyi Muazzam Kolah ve BAğımılicik Yapan "Sonic Pi" çılkto!

**Ne İşe Yarar?**
* **Çocuklara Kodlamayı (Döngüleri ve Thread'leri) Öğretmek:** Bir Çougca C dilinde "While Loop(Donfusüü)" u ANlatısasniz SıkıpıLr. Ama Sonic Pi De **"Kardeşiim, Eğer Bunu 'live_loop' İcine Kojarsdan Bateri Asla Duzmaka Surekli Calar!"** Dedişiniade Muzikle MAntigi Birlesitnrir.
* **Profesyoneller İçin Live Coding (Kulüp Performansları):** Şuan Dünyaca unlu "Algorave" (Algoritmik Rave - DJ siz Partilerllcer de) SanatciLarr Terminaleriniii Acar Ve Sahnnee Ruhy/SOonicpi kdolryula Tekna/Trance Beatleri Firkalir.

## Dilin Mantığı ve Kod Yapısı
Tamamen **Ruby** Sintaksıdır! Lakin Müzikal Bir Kışkıtmaya Sanihtnir. Noktalı virgül (;) Yokştur, Paratneze Gerek Yoktur. Kelimelrii Arda arde Dirzisin!
En Önmeli Farkı: Muzigin "Zamanini (Sleep/Bekle)" Komuttlarriyal SizYobntesniz.
`play 60` (Do notasinii Cal)
`sleep 1` (1 Vurus/Saniyee Bekle, Bosluk Bİrak, Sonrakiki Notasyaa GRc)

### Örnek Bir Sonic Pi Kodu: Bir DJ Gibi Bass(Bateri) Ritimi Ve Üstüne Synthesizer Çalan Canlı Algoritma!
Bir Partide Ekranna Su Kodlari Yazarrsaniiz CihaznniZdan (Asla Senkronu Kaybetmenen ve KensiniTekarlayan) Bir Müzik Çıkmayya başalt:

```ruby
# BU BIR SONIC PI (Ruby Mimarisi) KODUDUR 

# 1. BATERI (DRUMS) DONGUSU (Live Loop Kavrami - Sihirli Dir!)
# 'bateri' isminde aslas susmayacak bir DOnguu Kuruyozuz!
live_loop :bateri do
  
  # a) Bass Davuluna Vur (sample = Hazir Mp3/Ses dosyalarndan)
  sample :bd_haus             # (House Musik Kick'i/Güm Sesi)
  sleep 0.5                   # (Yarim Saniye Bekle)
  
  # b) Zil(Hi-Hat) Sesi VUr 
  sample :drum_cymbal_closed  # (Çıt Sesi)
  sleep 0.5                   # (Yarim Saniye Beklle.. Ve Tekrae BAasa dÖn!)
  
end

# 2. MELODI (SYNTHESIZER) DONGUSU - BATARI ILE AYNI ANDA CALIR(PARALEL/THREAD)!
live_loop :melodi do
  
  # Ne calacegini  (Syntheszizer turunu Sec : 'tb303' Efsanevi Acid-Bass synthdir!)
  use_synth :tb303
  
  # Bir Randoomlesterm (Rastgelelikk) KAtalim - Hicbir zAAMan sikici olmasin!
  # play komutu(Nota ccikarir), choose komutu ise Verilen Diijden (ArraydenD) rastgele Notav secero!
  play choose([38, 41, 50, 41, 62]), cutoff: rrand(60, 100), release: 0.2
  
  # Rastgee Note 0.25 ssniyeree(Ceyree) vurulsun!
  sleep 0.25
  
end

# SIZ BU KODU CALARKREN (Sarki devam ederken) 'choose' İcinedik Diziye Silep Basa Nola Yaaarzaanz
# Muzik Asla Durmaz, Yeni NOtaya "Kesintisizi Gecer (Live COding) !!"
```
Bu Muazzamdır Çünkii Szi (Programlama Öğrenrn bir İnsan Oalrakk); Asliinda Arksaplaanda **Miltu-Threading (2 Farkli İişlemin Paralelde Calismasi)** Olan o Çok Zor Yaziıımcı Koşusnunu Müzik İle Zihninze kaxmıs OlyorsuounZ! Baterialer (Thread 1), Meldoi(Thtread 2) Aynı Anda Calir ve Cökmz!

## Kimler Kullanır?
* Kkodlamalayi Çocuklaıra Veya Öğrencilrne **Sanat / Müzik** Uzerinedem AşılAamak Isteye Yüzzbinlercr Öğtemmen Ve Akademidysnen (Dunya çpaunida Meb Muferdaltarına girmisiitr).
* Elektronik (Techno / Acid) Şarkılar Üretererikn, SuperCollier'in Karnadjsıklıüğna Girmek  istemeyrn Ancak "Ruby"nin Temizlyigle MÜziikk Patlatmak İsttyen  **Modern Live-Coder(Canlı Performants Sanatçılarüı)**. (Sam Aaron Beyfendidn Kendi Yuoutube vidoalrinda Klüpleri Naşil Ayagaa kaalddidihi Gorurlebiülr!). Sanatsal ve Eglenceli En Mukkkeml İcdaddir.
