# D Dili (Dlang)

## Özet
D dili (Dlang), 2000'li yılların başlarında C++ dilinin en iyi bilinen özelliklerini (Donanım/Sistem erişim gücü ve OOP kapasitesi) koruyup, ona eziyet olan tarihi 1970 kalıntılarını (header dosyaları, karmaşık makrolar, çıldırmış pointer bellek yönetimleri) kazıyıp devirerek "C++'ın olması gereken temiz geleceği" hayaliyle yaratılmış modern bir dildir.

## Nedir ve Ne İşe Yarar?
1999 yılında efsanevi C++ derleyici mühendisi Walter Bright, "Artık C++ derleyicisi (compiler) kodlamak imkansız hale geliyor, dil çok kirlendi ve hantallaştı. Biz C++ enerjisine sahip; Java veya C# gibi modüllerle (klasörler, importlar) pürüssüz derlenen, her projenin çorba olmadığı yepyeni bir evren yaratmalıyız" dedi ve D dilini inşa etti.

C++ komut setiyle adeta evrimi iki nesil ileri sarmıştır.

**Ne İşe Yarar?**
* **Oyun Motorları / Yüksek Performanslı İşlemler:** Remake (Yeniden Yapım) oyun motorları tasarlanırken (Alan Wake Engine vb. alt araçları) çok yoğun matematik işlemlerinde Garbage Collector ile (veya isterseniz kapatarak saf tahsisata geçerek) C++ gücünde çalışır.
* **Üretkenlik (Mühendislik Pratiği):** C++ dilinde `Header (.h)` dosyalarını dahil etmek projenizde bir kabustur (Binlerce kere aynı dosya tekrar derlenir, makrolar isim çakışmasına girer, proje derlenmesi 1 saat falan sürer). D Dilinde `import` mantığı vardır; Java gibidir, devasa yüz bin satırlık program 2 saniyede tık diye derlenir, çuvallamaz.
* **Compile-Time Fonction Execution (CTFE):** En devrimsel gücü "Derleme Zamanı Fonksiyon Çalıştırma"dır. Yazdığınız kod "Daha siz EXE (Çalıştırılabilir dosya) butonuna bastığınız anda" bilgisayar arka planda karmaşık fonksiyon testlerini derleyicinin İÇİNDE çözüp sonucunu EXE'ye hapseder. Orijinal yazılım kullanıcının elindeyken artık o yorucu hesabı yapmaz bile.

## Dilin Mantığı ve Kod Yapısı
D dili, aynı C/C++ gibi aşağı katmanlara indiğinizde pointer (Gösterici) kullanmanıza, Assembly kodlarını araya yazmanıza (Inline-ASM) aynen izin verir. Fakat sizi buna mecbur bırakmaz.

İçerisinde gömülü (default) bir Çöp Toplayıcı (Garbage Collector) vardır. C/C++'ta `malloc/free` uykularınızı kaçırırken, D dilinde `new` ile nesneyi yaratıp geçersiniz. Hafıza ile işiniz bittiğinde C# veya Java gibi sistem arzu ettiği zaman kendi temizler. Ancak bu "oynatılamaz bir kural" değildir. İsterseniz `@nogc` (No Garbage Collection) etiketiyle fonksiyonu C dilini kıskandıracak manuel saf makine alanına indirebilirsiniz. 

**Örnek İşleyiş (Sembolik Olarak):**
Temelde C Ailesi (`{ }` süslü parantezli) dildir ama metaprogramlamada (kodlardan yeni kodlar yaratırken) ve şablonlarda (Templates) C++'ın okunamayan `<< >>` kaosunu atıp "Design by Introspection" gibi daha fonksiyonel (örneğin Range ve UFCS yapıları) sentaks kullanır.

### Örnek Bir D Kodu: İki Dünyanın Birleşimi
Büyük sistem uygulamaları için Garbage Collector ve okunaklı syntax farkını gösteren, bir yandan statik tiple donanım hızı vadeden bir "Sınıf" (Class) mimarisi prototipi:

```d
// C++'ın aksine eziyetli "include" mantığı yoktur, modern Import (Modül) sistemi kullanılır.
import std.stdio;   // Standart I/O modülü

// Bir nesne/sınıf yaratalım. Tıpkı C++ veya Java gibidir ama daha sadedir:
class UzayGemisi {
    private string  ism;
    private int     hiz;

    // "this" anahtar kelimesi ile D dilinin "Yapici" (Constructor) tanimi
    this(string isim, int maxHiz) {
        this.ism = isim;
        this.hiz = maxHiz;
    }

    void gazVer() {
        // C ve C++ daki karmaşık printf veya cout string birleştirmeleri yerine
        //writeln fonksiyonu direkt Java ya da C# estetiğinde çıktı verir.
        writeln(ism, " gemisi hizlaniyor! Su an: ", hiz, " isik hizi!");
    }
}

void main() {
    // Nesneyi Yarat. DİKKAT: C++'taki gibi bunu 'delete' edip silmenize gerek YOKTUR. 
    // D dilinin muhteşem Garbage Collector'u bunu (siz zahmet etmeden) iş bitimi arka planda imha edip RAM'e verir.
    UzayGemisi apollo = new UzayGemisi("Apollo", 300);
    
    // Yöntemi Çağır
    apollo.gazVer();

    // D'NİN GİZLİ GÜCÜ (@nogc Örneği):
    // Eger siz C veya Rust gibi "Burada cop toplayici asla duraksama (Lag) yapamaz, oyun kasar!" 
    // derseniz diye manuel bellek tahsis özgürlüğünü de hala elinzde tutarsiniz:
    
    import core.stdc.stdlib : malloc, free; // Doğrudan Saf C'den ithal edersin (C kütüphanesi uyumu Mükemmeldir)
    
    // Tamamen manuel, C++ gibi tehlikeli ve saf CPU hızında donanum pointer'ı:
    int* c_bellek_alani = cast(int*) malloc(int.sizeof * 10);
    if(c_bellek_alani != null) {
        free(c_bellek_alani); // Burayı BİZZAT siz serbest birakmak ZORUNDASİNİZ !
    }
}
```
İşte bu ikili doğası (Hem çok gelişmiş yüksek seviyeli dil lüksleri, hem saf C ile %100 uyumu) D'nin iskeletidir.

## Kimler Kullanır?
* C++'ın devasa derleme sürelerinden ve "Header/Macro" işkencelerinden kaçmak isteyen oyun programcıları ile, büyük kurumsal masaüstü sistemlerini sıfırdan revize etmek isteyen kurumsal yazılımcılar.
* Backend hizmetlerinin API sistemlerinde (WekaIO veya Remedy Entertainment gibi şirketler zaman zaman dev parçaları D lang altyapısı ile modernize etmişlerdir) çok hızlı ölçeklenip, C/C++ kütüphaneleriyle entegre çalışan çekirdek geliştiriciler.
