# Alice

## Özet
Alice; 1999 yılında Carnegie Mellon Üniversitesi (Dr. Randy Pausch - "Son Konuşma" efsanevi kitabının yazarı) liderliğindeki araştırmacılar tarafından, Nesne Yönelimli Programlamanın (OOP) temel direklerini (Sınıflar, Nesneler, Metotlar) lise ve üniversite öğrencilerine "Sıkıcı siyah metin ekranlarında değil; **3 Boyutlu Karakterlerin ve Dünyaların olduğu bir Masal Simülasyonunda Blokları Sürükterek**" anlatmak için devrimsel olarak geliştirilen 3D eğitim/betik sistemidir.

## Nedir ve Ne İşe Yarar?
1990'ların sonunda üniversitelerde Java ve C++ öğrenen gençler ilk dönemi "Ekrana Not Ortalaması Hesaplayan Yazıları(Konsol)" basarak geçiriyordu ve sınıftan sıkılıp kaçıyorlardı. Üstelik bir "Kapsülleme (Encapsulation)" veya "Nesne(Object)"'in ne demek olduğunu soyut olduğu için hayal edemiyorlardı.

Dr. Randy Pausch, "Öğrencilere Nesne (Object) nedir anlatmamız için, onlara GERÇEK BİR NESNE verelim" felsefesini kurdu. Alice yazılımında sahneye 3 Boyutlu Bir Tavşan (Nesne - Object) atarsınız. Tavşanın Rengi (Property - Nitelik) vardır. Tavşan zıplar (Method - Operasyon). Gençler Fareyle "Tavşan -> Zıpla()" bloğunu alır kodun içine atar "Play" dediği an ekrandaki O koca 3D Cisim Zıplamış olur.

**Ne İşe Yarar?**
* **Görsel Nesne Yönelimli Programlama (Visual OOP Eğitim):** Öğrenciler bilgisayarın arkasındaki o Soyut C++/Java kodlama eyleminin Aslında ne ifade ettiğini Kafalarında net bir fizik kuralları ile 3 boyutlu algılarlar. 
* **3D Hikaye/Oyun Anlatıcılığı:** Öğrenciler korkutucu hata metinleriyle(Exception) uğraşmak yerine, sahnede 3D modelleri hareket ettirip küçük film ve hikayeler kurarken arka planda "Döngü(While)"leri , "Paralele Eylemleri (Do Together)" öğrenir.

## Dilin Mantığı ve Kod Yapısı
Alice'te klasik manada (Klavye ile yazılan) bir sintaks/harf kodu YAZILMAZ (Özellikle Alice 2 ve 3). Scratch'e benzer şekilde **Sürükle-Bırak (Drag&Drop)** menüleri vardır, ama Alice çok daha "Java/C++" kılıklıdır. 

Yani Scratch'te "10 adım git" varken, Alice'te dümdüz `this.Tavşan.move(direction: FORWARD, amount: 2.0 meters)` gibi resmen "Java Kodu" şablonları içeren bloklar yer değiştirir. (Hatta kodlar tamamen geçerli Java kodudur arkaplanda).

**Örnek İşleyiş (Sembolik Olarak):**
Öğrenci Mouse ile Sürükler: [Do Together] Kutusu.
İçine Atar: [Tavşan -> Kolları -> Kaldır ()] VE [Kuş -> Kanat_Çırp ()]
"Play" dediğinde Java motoru bu nesne operasyonlarını Çoklu-işlem (Multi-threaded) mantığıyla aynı anda sahnede işletir.

### Örnek Bir Alice Mantığı (Koda Çevrilmiş Felsefesi): Java/C++ Eğitimi 
Alice'in arka planda Sürüklenen Blokların iç dünyasında tuttuğu ve gençlerin Java mantığına ezberlemeden adım attırdığı kurgu (Örneğin Penguenler ve Buzullar):

```text
// ALICE BLOK KODLAMA ŞABLONLARININ OKUNUR / YAZINIR HALİ:
// Eger kullanici Alice'de tavsani sürüklerse arkada bu Metodoloji tetiklenir:

// SAHNE / DÜNYA Mimarisi (World)
// 'myFirstMethod' : Program Play(Oynat) basildiginda ilk calisan C++ Main fonskiyonu rölü oynar.

public void myFirstMethod() {
    
    // 1. DÜŞÜNME (Variable)
    // Cikan Bulutcuğun icine "Merhaba Uzay!" metinini(string) ata:
    this.UzayliKarakteri.say("Merhaba Uzay Yaratiklari!");

    
    // 2. PARALEL İŞLEM (MULTI-THREADING / ASYNC) EGITIMI (DO TOGETHER)
    // Sadece Üniversitede gorulen Eşzamanlı İslem (Senkronizasyon) mantigini blokla ögretir!
    // Asagidaki tum emirleri AYNI AYNA Saniyede gerçekleştir:
    
    doTogether {
        
        // Uzayli gemisi YUKARI Dogru 5 Metre Kalksın
        this.UzayGemisi.move(MoveDirection.UP, 5.0 ); 
        
        // Pervanesi(Geminin alt objesi/Nesne hiyerarsisi) 2 Kere kendi etrafında Dönsun
        this.UzayGemisi.Pervane.roll(RollDirection.RIGHT, 2.0);
        
        // Uzayli Karaketi "Ha ha ha!" desin!
        this.UzayliKarakteri.say("Ha Ha Ha!");
        
    } // DoTogether Bitişi
    
    
    // 3. DÖNGÜ EGITIMI (WHILE) 
    // Uzay Gemisi hala Sahnedeyse (Yani oyun alanının dışına cikmadiysa)
    while(this.UzayGemisi.getDistanceTo(this.Kamera) < 100.0) {
        
        // Ileri gitmeyi sürdür (Lazo-motor eylemi)
        this.UzayGemisi.move(MoveDirection.FORWARD, 1.0);
    }
}
```
Öğrenci bu bloklardan tek bir süslü parantezi unutma korkusu yaşamadığı için; doğrudan "Obje nedir? Argüman nedir? Döngü Nedir? Metot ve Kalıtım nedir?" gibi mühendilsik dillerinin(C++/Java) çekirdeğini 3D animatörlük yaparak çözer.

## Kimler Kullanır?
* Evrendeki yenilikçi **Programlama Eğitmenleri, Liseler, Ortaokullar ve Üniversitelerin CS101 (Bilgi. Bilimlerine Giriş)** sınıfları.
* Özellikle Ortaokul/Lisede Scratch oynamış gençlere, direkt C++ siyah ekranına atmak yerine, Aradaki köprüyü (Java Obje mimarisi ile Scratch sürükleme köprüsünü) kuran mükemmel bir pedagojik ara istasyondur. Alice 3 Versiyonuyla çocukların kodları sonradan %100 "Java Dosyası" olarak İhraç(Export) edip gerçek NetBeans IDE'sinde derlemelerine olanak sağlamış dev bir mirastır.
