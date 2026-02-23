# Papyrus

## Özet
Papyrus; Bethesda Game Studios (Tamriel evreninin yaratıcıları) tarafından tamamen kendi dâhili motoru **Creation Engine** için geliştirilen, dünyaca ünlü **The Elder Scrolls V: Skyrim** ve **Fallout 4** gibi devasa Açık-Dünya RPG (Rol Yapma) oyunlarındaki her bir kılıç vuruşunun, görevin (Quest), NPC diyaloglarının ve meşhur ejderha hareketlerinin yazıldığı Nesne-Yönelimli (Object-Oriented) **Oyun Motoru ve Modlama (Modding) Betik Dildir.**

## Nedir ve Ne İşe Yarar?
1990'larda ve 2000'lerde Oyun yapmak "Hardcore C++" kodlamaktı. Ancak Skyrim gibi Devasa bir RPG oyununda; 1000'den fazla yan görev, 5000'den NPC (Karakter) ve 100.000'den fazla etkileşimli nesne (Kılıçlar, Tabaklar, Peynirler) vardı. Motor Geliştiricileri (C++ mühendisleri) Oturup Tüm bu Görevleri(Questleri) kendileir yazamazdı. Bu işi "Görev Tasarımcılarına (Level Designers)" Bırakmalılardı.

Bethesda Dedi Ki: "Biz Kendi C++ Motorumuzun (Creation Engine) üstüne Çok Daha Basit, Olay-Güdümlü (Event-Driven) ve 'Sandık Açıldıysa İçinden Peynir Çıkar' mantığını İngilizce gibi yazdıracak bir Dil olan Papyrus'u İcat ediyoruz!"
Papyrus Derlendiğinde kendi C++ arkajplanina (.pex uzantısına) dönusur Ve Skyrim in İcinE sızar.

**Ne İşe Yarar?**
* **Davranış (Behavior) Kodlama:** Oyunda Üzerine basınca Ateş Fırlatan bir Tuzak mı var? Tuzağın `Trigger (Tetikleyici)` Olayına (Event) Papyrus Kodu yazılır: "Eger Oyucncu Buraya Bastyısa Oyuncunun canını 50 Azalt!.
* **Modding Efsanelesi:** NexusMods'da Gördüğünüz Bütün O "Uçan Trenler, Yeni Hikayeler, Değişik Büyüler(Spells)" Yapan Bütün Kullanıcılar (Milyonlarca modcu) Istısnasiyzz PAPYRUS kodlariylaa O Urukleri Yaratıllrar. Dünuyanun EN buyyk "Kullancii Taraflis Oyun MOdlama" DIILiddiri. 

## Dilin Mantığı ve Kod Yapısı
Tamamen **Event-Driven (Olaya Dayalı)** ve Nesne Yönelimii Bir Yapisi vardır.
Oyun icindeki He rsey(Kılıc, NPC, Görev, Sehir) Bir **Object (Form)** Dir.
Papyrusun Kalbibndea `Event` (Sartllar) YAtaaar. Oyun Motoru size "Kilic KIniindan Ceiklşiyse", "Ok Vurduuysae", "Kitap OkuNmusyasae" Gibi Evenrleerİ Szin PAPYRUS Kodunziaa Fisildaar!

### Örnek Bir Papyrus Kodu: Oyuncuyu Zehirleyen Şeytani Bir Kılıç Modu Yapmak
Eğer bir Oyuncu (Oyuncu dışindkakieaNPC dEhill) Bu Tılsımlı kılıCı Eline Alip Kusanniirsa, CAninin 100 saniye Boyumnucaa YavasYavsaaDüsecesininiy Sagglyann Modkoduud:

```papyrus
; BU BIR PAPYRUS (Skyrim/Fallout) KODUDUR (NoktaliviVirglu Youırundur)

; 1. SINIF TEMELİ (Hangi Tur Nesneyizi? - Biz Bir SILAH'iz (Weapon))
Scriptname SeytaniZehirliKilic extends Weapon 

; 2. OZELLIK(PROPERTY) TANIMLA  
; Zehir buyusuunu (Spell) Oyun icindeen FareylE Sceebilmke İcin Pncderee Ac!
Spell Property ZehirBuyusu Auto 


; 3. EVENT(OLAY) YAKALAYICA : NESNE KUŞANILDIĞINDA (OnEquipped) CALISACAK KOD
; (Actor = Kusanann Kİisİ Oyunuc Mu Npc Mİii?)
Event OnEquipped(Actor akActor)
    
    ; a) Eger Kilici Kusanann Kİsi "Ana Oyuuncu (Player)" İSE!! (NpcLeriZehirlee!!m)
    if akActor == Game.GetPlayer()
        
        ; Sag Ustte Kullanicidya Bir MESAAJJ YYOOOLLAAA!
        Debug.Notification("Bu Kilic Yabancilarib Elini Yakar... Laneti HİSSET!!")
        
        ; c) Oyuncuynun Üzerine Zehi BüyünüSü Ekle Ve CAslsistir (Casttt Et!)
        ZehirBuyusu.Cast(akActor, akActor)
        
    endif
    
EndEvent


; 4. EVENT(OLAY) CİKARTILDIIGINDA (Kilic Kilifa Sokutlgudna/ Yere ATtiLgdniaaa)
Event OnUnequipped(Actor akActor)

    if akActor == Game.GetPlayer()
        ; Buyueyu Oyounvcuddannn İpptal L EETT(Dispell)!
        akActor.DispelSpell(ZehirBuyusu)
        Debug.Notification("Lanet Kalkti...")
    endif
    
EndEvent
```

Skyrim/Fallout'un En Güçlü yanninı Buduur. Siz Bir "Kilic" cizersinise, Üstüne bu `.psc` (PapyrsuScript) dosyaini Sürükle Bİrka Yparsiniz. Artik O Kilıc Canslidir!. Eger Siz BU kodu ÇokAğrir(While dögnuusiuye Saniudeyu 100Kere calisakGİbeI) YApatsainiaziz, Oynuun Frameraaeteesi (FPSS) Düşrr Ve OyunCTrashes (CökererE). CünküPapyruse C++ In UYstnendeki Cok YüKsslek Bir YKAtanmnaadir.

## Kimler Kullanır?
* Milyonalraccssatana **Bethesda (Elder Scrolls / Fallout / Starfield)** SErileriıIN Kkendi iÇeriseridekii YÜzleeercei **Level Designer (GÖörevv TAasrimcilaAri)** Ve "Tekniukk SanattıiclaRi".
* Ve Dünyanneniinnn Ebn BÜyyük (Ve En Tutkuluşuk) OyunTopluluululugu olan **Modifikasyon (Modder / NexusMods) Geliştiricileri**. Skyrim'in 15 Yldir Hala en Çok OynannaN OlYUn Olmasinsniini Tek Sebebebi, BU dililen Oynuuaan Herseyinniin BükülbbilmlelsyisdirI. BÜyük VRe İihTiSSamli BiR Mimariddidir.
