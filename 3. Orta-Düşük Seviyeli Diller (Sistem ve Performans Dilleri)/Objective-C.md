# Objective-C

## Özet
Objective-C (Obj-C); 1984 yılında Brad Cox ve Tom Love tarafından C diline Smalltalk'un şık ve muazzam Nesne Yönelimli Programlama (OOP - Mesajlaşma) kurgusunu eklemek için icat edilmiş, uzun yıllar boyunca kapalı kalıp sonradan Steve Jobs'ın NeXT şirketi ve ardından **Apple (Mac ve iOS)** ekosisteminin milyarlarca dolarlık ana motoru/dili olmuş efsanevi bir nesne yönelimli sistem dilidir.

## Nedir ve Ne İşe Yarar?
1980'lerde programcılar düz C dilini kullanırken donanım hızı muazzamdı ancak devasa grafik arayüzlü (GUI) uygulamalar yazarken C'nin OOP (Sınıf/Nesne) eksikliği projeleri çöktürüyordu.  Bjarne Stroustrup, C'nin üzerine katı sınıflar ekleyerek "C++"ı yarattı. 

Ancak Cox ve Love farklı bir yol seçti. Onlar C dilinin kalbine Smalltalk'ın o esnek, dinamik ve çalışma zamanında asılı(Runtime) "Nesnelerin Birbirine Mesaj Yollama (Message Passing)" dinamiğini lehimlediler. Objective-C, temelde "İçinde Smalltalk mesajları uçuşan saf C dili"dir. Hatta o kadar esnektir ki; Obj-C dosyasının (`.m` uzantılı) içine dümdüz standart C kodu da yazabilirsiniz, sistem tıkır tıkır ikisini birlikte çalıştırır.

**Ne İşe Yarar?**
* **Apple Ekosistemi (macOS ve iOS):** 2014 yılında Swift dili çıkana kadar, dünyadaki bütün iPhone, iPad ve Mac uygulamaları (WhatsApp'tan Instagram'a, Safari'den sistem çekirdeğine) %100 Objective-C ile yazılırdı. 
* **Cocoa Framework:** Apple'ın ekrandaki butonları, kaydırma çubuklarını ve pencereleri ekrandan sızdıran büyüsel kütüphanesinin (Foundation/Cocoa) var olma sebebidir. 

## Dilin Mantığı ve Kod Yapısı
Sözdizimi okuyanlara ilk başta "Bu nasıl dil?" dedirtecek kadar köşeli ve farklıdır. Metotları çağırmak Java/C#'taki gibi `araba.git()` şeklinde (Nokta ile) DEĞİLDİR. Smalltalk'tan alınan **Köşeli Parantez `[ ]` (Mesajlaşma)** kurgusu esastır: `[araba git]` şeklinde Nesnenin içine mesaj yollarsınız!

Cocoa (Apple) kütüphanelerinin tüm C-dışı eklentilerinin (Stringler, Arrayler vb) başına muhakkak At İşareti `@` konur. Eğer `""` derseniz bu C diline ait zavallı bir char dizisidir, lakin `@"Merhaba"` derseniz bu arkasında Apple'ın devasa hafıza yönetimi (ARC/Retain) koşan zırhlı bir `NSString` (Objective-C Objesi) olur. Aynı şekilde Sınıflar `@interface` ile açılır, `@implementation` ile yazılır.

**Örnek İşleyiş (Sembolik Olarak):**
Java'da bir rengi kırmızı yapmak: `kutu.rengiAyarla("Kirmizi", 50);`
Objective-C'de adeta düz yazı yazar gibi (Fakat parantez içinde gönderilir):
`[kutu renginiAyarla:@"Kirmizi" parlaklik:50];` (Parametrelerin önüne isimleri de yazılır ki şiir gibi olsun).

### Örnek Bir Objective-C Kodu: Arayüz ve Mesaj Paslaşması
C dilinin saf Hızını ve Pointer(`*`) zekasını, Smalltalk'ın o Köşeli Parantezli (Mesajlaşma) kurgusuyla harmanlayan klasik Apple (iOS/Mac) geliştirme şablonu:

```objective-c
// Objective-C de yorumlar C tabanli dillerdeki gibi '//' kullanilarak yapilir.

// Apple'in Ana Kutubhanesini Iceri Aktar (Header)
#import <Foundation/Foundation.h>

// 1. ADIM: ARAYUZ / HEADER TANIMLAMA (@interface)
// Object Sınıfı yaratıyoruz, kökü (NSObject) yani NeXTSTEP Objesi'nden miras alıyor.
@interface KahveMakinesi : NSObject

// Özellik (Property) Tanımlaması. pointer (*) kullanmak OOP sinifi oldugundan zorunludur!
@property (strong, nonatomic) NSString *kahveTuru;

// Metot (Mesaj) Tanimi. 
// Eksi (-) işareti bunun bir Instance (Nesne) metodu oldugunu gosterir:
- (void)kahveyiHazirlaVeDoldur:(int)fincanSayisi;

@end


// 2. ADIM: SINIFIN UYGULANMASI (@implementation)
@implementation KahveMakinesi

// Metodun Kodu Buraya Yazilir
- (void)kahveyiHazirlaVeDoldur:(int)fincanSayisi {
    
    // Klasik C dili mantigindaki If/Else eziyetsizce calisir:
    if (fincanSayisi > 0) {
        
        // Ekrana(Konsola) Ciktı Basma (NSLog). String önüne daima @ işareti!
        // self.kahveTuru (Class'in kendi ozelligi demektir)
        NSLog(@"Makine Calisti! %d fincan %@ hazirlaniyor...", fincanSayisi, self.kahveTuru);
    } else {
        NSLog(@"Hata: Lutfen fincan sayisi girin.");
    }
}
@end


// === 3. ADIM: PROGRAMIN ASIL CALISMA MOTORU (C dilindeki main) ===
int main(int argc, const char * argv[]) {
    // Hafizada Objelerin dagilmamasi icin Otomatik Referans Sayici blogunu (Autoreleasepool) acariz
    @autoreleasepool {
        
        // MUCİZE BATIYO ALIMI (Mesaj Gonderme - Smalltalk Kurgusu):
        // 1. Sınıfa 'alloc' (Hafizada yer ac) mesaji yolla
        // 2. Olusan hafizaya da 'init' (Baslat) mesaji yolla!
        KahveMakinesi *makinem = [[KahveMakinesi alloc] init];
        
        // Degiskene yazi ata (Apple'a has NSString @"" kurgusuyla)
        makinem.kahveTuru = @"Filtre Kahve";
        
        // OBJE'YE (makinem), ARGUMANLI (3) BIR METOT MESAJI YOLLA [ ]:
        [makinem kahveyiHazirlaVeDoldur:3];

    }
    return 0; // C makinesi temiz cikis.
}
```

Bu `[Obje Mesaj]` yapısı 30 yıl boyunca tüm iOS aplikasyonlarının beyni oldu. Arka planda devasa C pointerları, ön planda estetik metodolojiler yatıyordu.

## Kimler Kullanır?
* Evrendeki **Apple (Mac ve iOS/iPhone)** geliştiricilerinin neredeyse eski nesil olan tüm ekibi.
* 2014 Yılında Apple, Objective-C'nin bu köşeli parantezli yaşlı ve zor kurgusunu öldürüp yerine pürüzsüz C# modernliğindeki **Swift** dilini çıkardı. Dolayısıyla günümüzde yeni bir program asla Objective-C ile yazılmaz.
* Ancak, eski (Legacy) iOS ve Mac projelerini ayakta tutan şirket mühendisleri ve kök Apple Framework çekirdeğini (UIKit vb.) güncelleyen mimarlar hala kullanmaktadır. (Günümüzde %100 "Eski Toprak / Legacy" olarak anılır).
