# Papyrus

## Özet
Papyrus; Bethesda Game Studios (Tamriel evreninin yaratıcıları) tarafından tamamen kendi dâhili motoru **Creation Engine** için geliştirilen, dünyaca ünlü **The Elder Scrolls V: Skyrim** ve **Fallout 4** gibi devasa Açık-Dünya RPG (Rol Yapma) oyunlarındaki her bir kılıç vuruşunun, görevin (Quest), NPC diyaloglarının ve meşhur ejderha hareketlerinin yazıldığı Nesne-Yönelimli (Object-Oriented) **Oyun Motoru ve Modlama (Modding) Betik Dildir.**

## Nedir ve Ne İşe Yarar?
1990'larda ve 2000'lerde Oyun yapmak "Hardcore C++" kodlamaktı. Ancak Skyrim gibi Devasa bir RPG oyununda; 1000'den fazla yan görev, 5000'den NPC (Karakter) ve 100.000'den fazla etkileşimli nesne (Kılıçlar, Tabaklar, Peynirler) vardı. Motor Geliştiricileri (C++ mühendisleri) Oturup Tüm bu Görevleri(Questleri) kendileir yazamazdı. Bu işi "Görev Tasarımcılarına (Level Designers)" Bırakmalılardı.

Bethesda Dedi Ki: "Biz Kendi C++ Motorumuzun (Creation Engine) üstüne Çok Daha Basit, Olay-Güdümlü (Event-Driven) ve 'Sandık Açıldıysa İçinden Peynir Çıkar' mantığını İngilizce gibi yazdıracak bir Dil olan Papyrus'u İcat ediyoruz!"
Papyrus Derlendiğinde kendi C++ arka planına (.pex uzantısına) dönüşür Ve Skyrim'in içine sızar.

**Ne İşe Yarar?**
* **Davranış (Behavior) Kodlama:** Oyunda Üzerine basınca Ateş Fırlatan bir Tuzak mı var? Tuzağın `Trigger (Tetikleyici)` Olayına (Event) Papyrus Kodu yazılır: "Eğer Oyuncu Buraya Bastıysa Oyuncunun canını 50 Azalt!"
* **Modding Efsanelesi:** NexusMods'da Gördüğünüz Bütün O "Uçan Trenler, Yeni Hikayeler, Değişik Büyüler (Spells)" Yapan Bütün Kullanıcılar (Milyonlarca modcu) İstisnasız PAPYRUS kodlarıyla o ürünleri yaratırlar. Dünyanın en büyük "Kullanıcı Taraflı Oyun Modlama" dilidir. 

## Dilin Mantığı ve Kod Yapısı
Tamamen **Event-Driven (Olaya Dayalı)** ve Nesne Yönelimli Bir yapısı vardır.
Oyun içindeki her şey (Kılıç, NPC, Görev, Şehir) Bir **Object (Form)**dir.
Papyrus'un kalbinde `Event` (Şartlar) yatar. Oyun Motoru size "Kılıç Kınından Çekildiyse", "Ok Vurduysa", "Kitap Okunduysa" Gibi eventleri sizin PAPYRUS kodunuza fısıldar!

### Örnek Bir Papyrus Kodu: Oyuncuyu Zehirleyen Şeytani Bir Kılıç Modu Yapmak
Eğer bir Oyuncu (Oyuncu dışındaki NPC değil) Bu Tılsımlı kılıcı Eline Alıp Kuşanırsa, Canının 100 saniye boyunca yavaş yavaş düşmesini sağlayan mod kodu:

```papyrus
; BU BİR PAPYRUS (Skyrim/Fallout) KODUDUR (Noktalı virgül yorumdur)

; 1. SINIF TEMELİ (Hangi Tür Nesneyiz? - Biz Bir SİLAH'ız (Weapon))
Scriptname SeytaniZehirliKilic extends Weapon 

; 2. ÖZELLİK (PROPERTY) TANIMLA  
; Zehir büyüsünü (Spell) Oyun içinden Fareyle Seçebilmek İçin Pencere Aç!
Spell Property ZehirBuyusu Auto 


; 3. EVENT (OLAY) YAKALAYICI: NESNE KUŞANILDIĞINDA (OnEquipped) ÇALIŞACAK KOD
; (Actor = Kuşanan Kişi Oyuncu mu NPC mi?)
Event OnEquipped(Actor akActor)
    
    ; a) Eğer Kılıcı Kuşanan Kişi "Ana Oyuncu (Player)" İSE!! (NPC'leri Zehirleme!!)
    if akActor == Game.GetPlayer()
        
        ; Sağ Üstte Kullanıcıya Bir MESAJ YOLLA!
        Debug.Notification("Bu Kılıç Yabancıların Elini Yakar... Laneti HİSSET!!")
        
        ; c) Oyuncunun Üzerine Zehir Büyüsünü Ekle Ve Çalıştır (Cast Et!)
        ZehirBuyusu.Cast(akActor, akActor)
        
    endif
    
EndEvent


; 4. EVENT (OLAY) ÇIKARTILDIĞINDA (Kılıç Kınına Sokulduğunda / Yere Atıldığında)
Event OnUnequipped(Actor akActor)

    if akActor == Game.GetPlayer()
        ; Büyüyü Oyuncudan İptal ET (Dispel)!
        akActor.DispelSpell(ZehirBuyusu)
        Debug.Notification("Lanet Kalktı...")
    endif
    
EndEvent
```

Skyrim/Fallout'un En Güçlü yanını Budur. Siz Bir "Kılıç" çizersiniz, Üstüne bu `.psc` (PapyrusScript) dosyasını Sürükle Bırak yaparsınız. Artık o kılıç canlıdır!. Eğer siz bu kodu çok ağır (While döngüsüyle saniyede 100 kere çalışacak gibi) yaparsanız, oyunun kare hızı (FPS) düşer ve oyun çöker. Çünkü Papyrus, C++'ın üstündeki çok yüksek bir katmandır.

## Kimler Kullanır?
* Milyonlarca satan **Bethesda (Elder Scrolls / Fallout / Starfield)** serilerinin kendi içerisindeki yüzlerce **Görev Tasarımcısı (Level Designer)** Ve "Teknik Sanatçılar".
* Ve Dünyanın en büyük (Ve en tutkulu) oyun topluluğu olan **Modifikasyon (Modder / NexusMods) Geliştiricileri**. Skyrim'in 15 yıldır hala en çok oynanan oyun olmasının tek sebebi, bu dille oyunun her şeyinin bükülebilmesidir. İhtişamlı bir mimaridir.
