# Sonic Pi

## Özet
Sonic Pi; 2012 yılında Cambridge Üniversitesi'nden Dr. Sam Aaron tarafından (Aslen Raspberry Pi mikrobilgisayarlarında çocuklara yazılımı sevdirmek amacıyla) icat edilen; ancak zamanla devasa bir büyüme yaşayarak SuperCollider'ın karmaşıklığını "Ruby Dilinin Efsanevi Okunabilirliğiyle" harmanlayan, Dünyadaki en erişilebilir, eğlenceli ve güçlü **Eğitim Odaklı Canlı Performans (Live Coding / Algorave) Müzik Dilidir.**

## Nedir ve Ne İşe Yarar?
SuperCollider harika bir Dildi ama Sintaksı (Yazım Şekli) SmallTalk kökenli olduğu için İlkokul/Lise veya Müzik-Yazılımına Yeni başlayacak İnsanları "C++ gibi Hatalarla" Korkutuyordu. (Parantezler, Süslü Oklar, Sınıflarla Adam Boğuluyordu).

Sam Aaron Dediki: **Öyle bir Dil Yapacağım ki, Çocuklar sadece İngilizce 'Çal 60' (play 60) Yazacak ve Müzik Başlayacak! Ama O dili İsteyen Yetişkin BiR DJ Alıp, Arkada SuperCollider Motoruna Bağlanarak Gece Kulübünü sallayabilecek!**
Dilin tabanını (Yazılım Arayüzünü) Okunabilirliğiyle Meşhur OLAN **Ruby** Diliyle İnşa etti, Ama "Sesi Çıkartacak" Olan Kalbini (Scsynth - SuperCollider Engine) olarak Ayarladı. Ortaya Oynanması Muazzam Kolay ve Bağımlılık Yapan "Sonic Pi" çıktı!

**Ne İşe Yarar?**
* **Çocuklara Kodlamayı (Döngüleri ve Thread'leri) Öğretmek:** Bir Çocuğa C dilinde "While Loop (Döngüsünü)" anlatırsanız Sıkılır. Ama Sonic Pi De **"Kardeşim, Eğer Bunu 'live_loop' İçine Koyarsan Bateri Asla Durmaksızın Sürekli Çalar!"** dediğinizde Müzikle Mantığı Birleştirir.
* **Profesyoneller İçin Live Coding (Kulüp Performansları):** Şuan Dünyaca ünlü "Algorave" (Algoritmik Rave - DJ'siz Partilerde) Sanatçılar Terminallerini Açar Ve Sahne Ruby/Sonic Pi kodlarıyla Techno/Trance Beatleri Fırlatır.

## Dilin Mantığı ve Kod Yapısı
Tamamen **Ruby** Sintaksıdır! Lakin Müzikal Bir Kışkıtmaya Sahiptir. Noktalı virgül (;) Yoktur, Paranteze Gerek Yoktur. Kelimeleri Arda arda Dizersin!
En Önemli Farkı: Müziğin "Zamanını (Sleep/Bekle)" Komutlarıyla Siz Yönetirsiniz.
`play 60` (Do notasını Çal)
`sleep 1` (1 Vuruş/Saniye Bekle, Boşluk Bırak, Sonraki Notaya Geç)

### Örnek Bir Sonic Pi Kodu: Bir DJ Gibi Bass(Bateri) Ritimi Ve Üstüne Synthesizer Çalan Canlı Algoritma!
Bir Partide Ekrana Şu Kodları Yazarsanız Cihazınızdan (Asla Senkronu Kaybetmeyen ve Kendini Tekrarlayan) Bir Müzik Çıkmaya başlar:

```ruby
# BU BIR SONIC PI (Ruby Mimarisi) KODUDUR 

# 1. BATERI (DRUMS) DONGUSU (Live Loop Kavrami - Sihirli Dir!)
# 'bateri' isminde asla susmayacak bir Döngü Kuruyoruz!
live_loop :bateri do
  
  # a) Bass Davuluna Vur (sample = Hazir Mp3/Ses dosyalarndan)
  sample :bd_haus             # (House Müzik Kick'i/Güm Sesi)
  sleep 0.5                   # (Yarim Saniye Bekle)
  
  # b) Zil(Hi-Hat) Sesi Vur 
  sample :drum_cymbal_closed  # (Çıt Sesi)
  sleep 0.5                   # (Yarim Saniye Bekle.. Ve Tekrar Başa Dön!)
  
end

# 2. MELODİ (SYNTHESIZER) DÖNGÜSÜ - BATERİ İLE AYNI ANDA ÇALIR (PARALEL/THREAD)!
live_loop :melodi do
  
  # Ne çalacağını (Synthesizer türünü Seç : 'tb303' Efsanevi Acid-Bass synthdir!)
  use_synth :tb303
  
  # Bir Randomlaştırma Katılımı Katılım - Hiçbir zaman sıkıcı olmasın!
  # play komutu (Nota çıkartır), choose komutu ise Verilen Diziden rastgele nota seçer!
  play choose([38, 41, 50, 41, 62]), cutoff: rrand(60, 100), release: 0.2
  
  # Rastgele Nota 0.25 saniyede (Çeyrek) vurulsun!
  sleep 0.25
  
end

# SİZ BU KODU ÇALIŞTIRIRKEN (Şarkı devam ederken) 'choose' İçindeki Diziye Silip Yeni Nola Yazarsanız
# Müzik Asla Durmaz, Yeni Notaya "Kesintisiz Geçer (Live Coding) !!"
```
Bu Muazzamdır Çünkü Siz (Programlama Öğrenen bir İnsan Olarak); Aslında Arka Planda **Multi-Threading (2 Farklı İşlemin Paralelde Çalışması)** Olan o Çok Zor Yazılımcı Konusunu Müzik İle Zihninize kazımış Oluyorsunuz! Bateriler (Thread 1), Melodi (Thread 2) Aynı Anda Çalır ve Çökmez!

## Kimler Kullanır?
* Kodlamayı Çocuklara Veya Öğrencilerine **Sanat / Müzik** Üzerinden aşılamak İsteyen Yüzbinlerce Öğretmen Ve Akademisyen (Dünya çapında MEB müfredatlarına girmiştir).
* Elektronik (Techno / Acid) Şarkılar Üretirken, SuperCollider'ın Karmaşıklığına Girmek İstemeyen Ancak "Ruby"nin Temizliğiyle Müzik Patlatmak İsteyen **Modern Live-Coder (Canlı Performans Sanatçıları)**. (Sam Aaron Beyfendinin Kendi YouTube videolarında kulüpleri nasıl Ayağa Kaldırdığı görülebilir!). Sanatsal ve Eğlenceli En Mükemmel İcattır.
