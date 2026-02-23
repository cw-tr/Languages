# C++

## Özet
C++, efsanevi C dilinin makineye olan mutlak yakınlığını ve saf hızını alıp, üzerine karmaşık "Nesne Yönelimli Programlama" (OOP - Object Oriented) konseptlerini ve yüksek seviyeli dil konforunu ekleyerek inşa edilmiş performans odaklı bir "hibrit" devdir.

## Nedir ve Ne İşe Yarar?
1979'da Bjarne Stroustrup, doktora tezi için çok karmaşık simülasyonlar yazmak istediğinde C dilinin hantal "düz (prosedürel) mantığından" bunalmıştı. Programlanan her şeyi tıpkı gerçek hayattaki "Nesneler" gibi hayal edebileceği (Örneğin bir arabanın rengi, hızı, motor gücü) özellikler ekledi ve buna ilk olarak "Sınıflı C" (C with Classes) adını verdi. Daha sonra dil kendini aşarak C++ oldu.

**Ne İşe Yarar?**
* **AAA Kalite Oyun Motorları:** Hızın mikrosaniyelere bölündüğü, milyarlarca poligonun işlendiği tüm büyük oyun motorları (Unreal Engine, CryEngine, Frostbite vb.) ve modern konsol, bilgisayar oyunları tamamen C++ ile yazılır.
* **Yüksek Performanslı Masaüstü Yazılımları:** Adobe Photoshop, Autodesk AutoCAD, Google Chrome (V8 Motoru) ve web tarayıcıları, kaynak tüketimi çok ağır olduğu için gücü doğrudan donanımdan çekmek adına C++ kullanır.
* **Finansal (HFT) Ticaret:** Borsalarda (Wall Street) nanosaniye hızında milyonlarca dolarlık alış-satış yapan otomatik ticaret botları, Java veya C#'ın "Çöp Toplayıcısı"nın duraksamasından (Garbage Collector Pause) nefret ettiği için sıfır gecikmeli C++'a muhtaçtır.

## Dilin Mantığı ve Kod Yapısı
C++ tamamen C ile geriye dönük uyumludur; yani eski C kodlarını da derler, C'deki o tehlikeli *Pointer* ve *Manuel Bellek (`new`/`delete` veya `malloc`/`free`)* çılgınlığını korur. 

İlkesi şu meşhur felsefedir: **"Ödemediğin (kullanmadığın) şeyin bedelini ödemezsin (Zero-cost abstractions)."** Yüksek seviyeli (örneğin Java'daki gibi) sınıflar ve kalıtım oluşturabilirsiniz ancak bu lüks yapı, derleyici tarafından derlenirken arkada makine kodunda ekstra performans kaybı yaratmayacak (işlemciyi bir saniye bile geciktirmeyecek) şekilde C benzeri sıfır bedelli yapıya düzleştirilir.

**Örnek İşleyiş (Sembolik Olarak):**
C'de sadece işlevleriniz vardır (`vur()`, `atla()`). C++ ile devasa bir "Asker" sınıfı (`class`) yaratıp askerin canını (HP) ve zırhını bu sınıfın *içinde* paketlersiniz (Kapsülleme - Encapsulation). Ardından o askeri bir `Pointer` ile RAM'de manuel olarak fiziksel yere tahsis edersiniz. Böylece hem modern tasarım kurallarına uyan geniş bir ekip ile uyumlu çalışabilir hem de C hızında saf donanım yeteneği kullanırsınız.

### Örnek Bir C++ Kodu: OOP ve Manuel Bellek
Hem bir obje tasarlayıp hem de "RAM benden sorulur" diyen o kesişim noktasına bakalım:

```cpp
#include <iostream>
#include <string>

// C dilinde olmayan Sınıf (Class) yapısı: Arabanın özelliklerini birleştiriyoruz.
class Araba {
private:
    std::string model;
    int beygir_gucu;

public:
    // Yapıcı Fonksiyon (Constructor): Nesne oluşturulduğunda tetiklenir
    Araba(std::string m, int bg) {
        model = m;
        beygir_gucu = bg;
        std::cout << model << " hattan indirildi! (Bellekte yaratildi)\n";
    }

    // Yıkıcı Fonksiyon (Destructor): Nesne silindiğinde (free) tetiklenir
    ~Araba() {
        std::cout << model << " hurdaliga gitti! (Bellekten silindi)\n";
    }

    void gaz_ver() {
        std::cout << model << " VROOM! (" << beygir_gucu << " BG guc devrede)\n";
    }
};

int main() {
    /* İŞTE C++ FARKI: Ram'in HEAP bölgesinde DOĞRUDAN 'new' ile yer tahsis et (C'deki malloc gibi) */
    Araba* benimArabam = new Araba("Ford Mustang", 450);

    /* Pointer üzerinden nesnenin eylemlerini tetikle */
    benimArabam->gaz_ver();

    /* Çöp toplayıcısı YOKTUR. Eğer bu nesneyi 'delete' etmezseniz bellek sızıntısı olur! */
    delete benimArabam; // Bu komut çalışır çalışmaz Yıkıcı Fonksiyon (~Araba) tetiklenir.

    return 0;
}
```

## Kimler Kullanır?
* AAA Oyun Stüdyoları ve Grafik/Fizik Motoru (Engine) Mühendisleri (Örn: Epic Games).
* Yüksek Frekanslı Ticaret (High-Frequency Trading) sistemleri mimarları, Kantitatif yazılımcılar.
* Veri biliminin ağır arka plan motorlarını yazan (TensorFlow vb. C++ ile yazılıp Python forması giydirilmiştir) performans mühendisleri ve Otonom Araç yazılımcıları.
