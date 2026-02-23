# Simula

## Özet
Simula (Simula 67); 1960'larda Norveç Bilgi İşlem Merkezi'nde Kristen Nygaard ve Ole-Johan Dahl tarafından icat edilen, yazılım mühendisliğini kökünden değiştirerek dünyadaki **"Nesne Yönelimli Programlamayı (OOP - Object Oriented Programming)"** Sınıf(Class), Miras(Inheritance) ve Obje(Object) kavramlarını icat ederek teknoloji tarihine kazıyan evrenin en saygın tarihi dillerinden biridir.

## Nedir ve Ne İşe Yarar?
1960'larda nükleer reaktörler, lojistik depolar veya yollardaki trafik akışı gibi karmaşık sistemler "Simüle" edilmeye (Deneyleri bilgisayarda test edilmeye) çalışılıyordu. Mevcut diller olan ALGOL veya Fortran ile, bir otoyoldaki "1000 farklı Arabayı" kodlamak demek 1000 farklı değişken yaratıp spagetti kodun içinde boğulmak demekti.

Norveçli bilim adamları devasa bir uyanış yaşadı: "Dünya dümdüz kodlardan ibaret değil. Dünyada *Kavramlar (Sınıflar)* ve bu kavramlardan üretilen *Şeyler (Objeler)* vardır!" Araba bir Kalıptır (Class/Sınıf); Ford veya Toyota bu kalıptan çıkan bir Eşyadır (Object/Obje). Her Obje kendi Rengine(Özelliğine/Verisine) ve kendi Motor Çalışma mekanizmasına (Metoduna) sahip olmalıydı! 

İşte Simula, C++ veya Java ortada yokken "KALITIM (Inheritance), SINIF (Class), ve POLYMORPHISM" kavramlarını tümüyle o gün keşfedip dil olarak yazan efsanedir.

**Ne İşe Yarar?**
* **Olay Odaklı Simülasyon (Discrete Event Simulation):** Lojistik depoların yönetim planı veya Nükleer tepkimelerin modellenmesindeki nesneleri kendi özellikleriyle ayaktatutar. Zaten adını (Simulation Language) kelimelerinden alır.
* **C++'ın İlham Kaynağı:** C++ dilinin yaratıcısı Bjarne Stroustrup, doktora tezini yazarken "Simula'nın Muazzam Sınıf/Nesne zekasını keşke C'nin hızıyla birleştirebilseydim" deyip *"C with Classes"* (Sonradan adı C++ oldu) modelini icat etmiştir.

## Dilin Mantığı ve Kod Yapısı
Simula, alt taban (Base) olarak tamamen **ALGOL** dilini (`begin..end` yapısını) kullanır, ama bunun üzerine yepyeni devrimsel "Sınıf" mimarisini ekler.

Siz bir `Class` yaratırsınız. İçinde sadece veri değil (Geleneksel Struct gibi), PROSEDÜRLER (Metotlar / Fonksiyonlar) de iç içe gömülür. Ve işin en çılgın kısmı: Bir Class, başka bir Class'ın **Özelliklerini Miras Alabilir (Inheritance)!** (Örn: `Link CLASS Araba` derseniz, Araba, Link sınıfının bütün yetki ve fonksiyonlarını çalar ve kendi üstüne giyer. Evrendeki ilk kalıtımdır).

**Örnek İşleyiş (Sembolik Olarak):**
Java'daki `class Kopek extends Hayvan` hiyerarşisini, Java doğmadan 30 sene evvel `Hayvan CLASS Kopek` diyerek inşa etmek ve bunu bir Çöp Toplayıcıyla (Garbage Collector) hafızadan otomatik silmek!

### Örnek Bir Simula Kodu: Evrenin İlk Sınıfı ve Miras Alımı (Inheritance)
Modern OOP yazılımcısı (Java/C# vb.) gördüğünde "Vay canına, biz o mirastan C++ üzerinden gelmişiz" diyeceği, 1967 yılındaki o efsanevi (Sınıf ve Nokta operatörü) yapısı:

```simula
! Simula dilinde Yorum Satırları Ünlem İşareti (!) ile başlar ;

BEGIN
   ! Temel Ana (Super) Sinifi Yaratiyoruz ;
   CLASS Araba(KapiSayisi, MaksHiz);
      INTEGER KapiSayisi, MaksHiz;
   BEGIN
      ! Arabanin Iç Fonksiyonu (Methodu) ;
      PROCEDURE KornaCal;
      BEGIN
         OutText("Bip Biip!");
         OutImage; ! OutImage Simula'da ekrana basi ve satiri yeniler ;
      END KornaCal;
   END Araba;

   ! EVRENIN İLK KALITIMI (INHERITANCE / EXTENDS): 
   ! 'SporAraba' sinifi, 'Araba'nin tum kanini çeker(Kapi, MaksHiz korna) ve ustune TURBO ekler! ;
   Araba CLASS SporAraba(TurboCarpan);
      INTEGER TurboCarpan;
   BEGIN
      ! Spor Arabaya Ozel Kendi Metodu ;
      PROCEDURE HizGor;
      BEGIN
         OutText("Normal Hizi: "); OutInt(MaksHiz, 4);
         OutText(" | Turboyla Hizi: "); OutInt(MaksHiz * TurboCarpan, 4);
         OutImage;
      END HizGor;
   END SporAraba;

   ! --- NESNELERI (OBJECTS) YARATMA VE HAFIZAYA ASMA ZAMANI --- ;
   ! Referans Çubuğu (Pointer) yaratiyoruz;
   REF(Araba) benimArabam;
   REF(SporAraba) ralliArabam;

   ! "NEW" Kelimesinin icat edildigi, Evrende Ilk Obje yaratimi ! ;
   benimArabam :- NEW Araba(4, 180); 
   ralliArabam :- NEW SporAraba(2, 250, 2); ! Kapi: 2, Hiz: 250, Turbo: 2 Carpan ;

   ! Objelerin Icindeki Nokta(.) Operatörü ile Fonksiyon Cagirma ;
   benimArabam.KornaCal;
   
   ! Ralli arabasi hem kendi metodunu, hem de Babadan (Araba'dan) caldigikornayi calabilir! ;
   ralliArabam.HizGor;
   ralliArabam.KornaCal;

END
```

Hafızada Referans gösterimi (`:-` sembolü) ile nesnelerin Heap'te saklanması 60'lar için bilimkurgu düzeyinde bir teknolojiydi.

## Kimler Kullanır?
* C++ icat edildikten sonra maalesef Simula tamamen piyasasından koparıldı. Asıl amaç olan Simülasyon gücü yerini, Genel Amaçlı Performans dillerine (C++) bıraktı. 
* Şuan 2020'ler itibariyle %100 oranında **ölü ve tarihi bir mirastır.**
* Ancak Alan Kay (Smalltalk mucidi) ve Bjarne Stroustrup (C++ mucidi) sayesinde "Nesne Yönelimli Programlama" akımı bu dilden çıkıp evreni ele geçirmiştir.
