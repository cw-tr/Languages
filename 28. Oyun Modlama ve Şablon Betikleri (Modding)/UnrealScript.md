# UnrealScript (UScript)

## Özet
UnrealScript (UScript); 1998 yılında Epic Games'in kurucularından "Tim Sweeney" (Bugünün Milyarderi) tarafından efsanevi **Unreal Engine 1, 2 ve 3** oyun motorları (Unreal Tournament, Gears of War, Bioshock, Batman Arkham serisi) için özel olarak yaratılmış olan, Syntax'ı doğrudan Java'dan kopyalanmış, C++'ın zorluklarından (Pointerlar/Bellek Sızıntıları) programcıları kurtarıp SADECE "Oyun Mantığı (Gameplay/Silahlar)" yazmaya odaklayan efsanevi, ancak bugün **Terkedilmiş (Ölü Dil)** Nesne Yönelimli Oyun Dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda DOOM, Quake gibi oyunları Yazan John Carmack her şeyi **C (veya C++)** diliyle yazıyordu. Ama C çok zordur. Bir silahı ateşlediğinizde "Merminin RAM'den silinmesini (Memory Free) unutursanız", oyun 10 dakika sonra RAM şişkinliğinden çökerdi (Memory Leakler).

Tim Sweeney Dedi Ki: **"Benim Oyun Motorumun (Unreal) Alt Çekirdeği (Fizik motoru, Render/Grafikleri) C++ olacak! Ama siz o katmanları ellemeyeceksiniz! Ben size 'UnrealScript' diye bir dil veriyorum. Java/C++ benzeridir. İçindeki mermiyi RAM'den silmeyi ben (Garbage Collector) yapacağım. Siz sadece OYUN YAPMAYA odaklanın!"**

**Ne İşe Yarar?**
* **Gameplay (Oynanış) Kodlaması:** Oyuncu (Pawn/Controller), Silahlar (Weapons), Yapay zeka (AI/Bot) Ve Oyun Kuralları (Game Rules); Hepsi UnrealScript ile yazılırdı.
* O yılların en büyük modlaması (UnrealEd) ve muhteşem çapraz platform (PC/Console) bağımsızlığı sağladı.

## Dilin Mantığı ve Kod Yapısı
Java'ya %95 oranında benzer. Her dosyayı (Silahı ya da NPC'yi) ayrı bir `Class` olarak tanımlardı. 
Dilin en büyük devrimi: **States (Durum Makinesi / FSM)** idi. Oyunlarda bir karakter (Pawn) "Uyuyor", "Ateş Ediyor" ya da "Kaçıyor" olabilir. Tim Sweeney "State (Durum)" kavramını dilin temeline yerleştirmiştir.

### Örnek Bir UnrealScript Kodu: Bir Roketatar (Rocket Launcher) Silahı Yapmak
UnrealScript'te bir silahın ateşlenmesi ve mermisinin (Roket) havada kodlanması:

```unrealscript
// BU BIR UNREALSCRIPT (Unreal Engine 1-3) KODUDUR

// 1. SINIF TANIMLAMASI: Benim Silahım (Weapon) sınıfından türeyen bir yeni Roketatarım
class SuperRoketar extends Weapon;

// Varsayılan Özellikleri Ayarla (C++'daki Default Properties Bloğu)
defaultproperties
{
    ItemName="Ölüm Meleği Roketi"                 // Adamın silahı alınca göreceği isim
    AmmoClass=Class'RoketatarMermisi'             // Kullanacağım mermi türü
    FireRate=1.5                                  // Saniyede kaç kere ateş edebilir? (Yavaş bir silah)
    Damage=250                                    // Merminin vuruş gücü (Oldukça ölümcül)
}

// 2. ATEŞ ETME ETKİNLİĞİ (Fonksiyon)
// Oyuncu Mouse Sol Tıkladığında çalışacak fonksiyon
function Fire(float Value)
{
    // EĞER mermim varsa (Boş değilse)
    if ( AmmoType.UseAmmo(1) ) 
    {
        // Ses çal: Ateş sesini çıkar!
        PlaySound(FireSound, SLOT_None, 4.0);
        
        // Projectile (MERMİ)'yi namludan fırlat!
        // Spawn(YARAT) emriyle roketi o yöne doğru uçur!
        Spawn(class'RoketatarMermisi', self,, StartLocation, AimRotation);
        
        // Ateş etme animasyonunu çağır!
        PlayAnim('Fire', Default.FireRate, 0.0);
    }
}
```

Bu dili yazan oyun geliştiricisi, merminin çalıştıktan ve yere çarptıktan sonra, "Ben bu mermiyi RAM'den sileyim" diye kendini yormaz. UnrealScript'in Garbage Collector'ı onu otomatik siliyor!


## Kimler Kullanır?
* 1998 ve 2014 yılları arasında (UE1, UE2, UE3) motorlarıyla oyun yapan bütün **oyun geliştiricileri ve mod geliştiricileri**. 
* **NEDEN ÖLÜ BİR DİLDİR?**: 2013 yılında Epic Games; Unreal Engine 4'ü (UE4) piyasaya sürdüğünde; **UnrealScript'i tamamen çöpe attı!** Yerine **C++ (Saf güç)** ve **Unreal Blueprints (Görsel betikleme - Bakınız Level 16)** sistemlerini getirdi. Bu yüzden bu efsanevi dil tarihin karanlık sayfalarına gömüldü.
