# Haxe

## Özet
Haxe; 2005 yılında Nicolas Cannasse tarafından icat edilen, dünyadaki diğer yüzlerce dile göre inanılmaz "farklı bir hedefe" sahip olan; kendisi bilgisayarda veya sunucuda doğrudan bir şey yapmaya çalışmak yerine **"Beni bir kez yazın (Strict-Type OOP formunda), ben sizi arka planda Java'ya, C++'a, Python'a, JavaScript'e, PHP'ye ve C#'a AYNI ANDA ÇEVİRİRİM (Cross-Compiler)"** diyerek ortaya çıkmış ve "Dil Çevirici (Evrensel Çevirmen / Transpiler)" şahikası olan efsanevi dildir.

## Nedir ve Ne İşe Yarar?
2000'lerin başlarında Flash (ActionScript) internete hakimdi. Lakin mobil cihazlar (iPhone falan) çıkarken Flash'ı reddetti. Bir oyun şirketi hem iOS için (Objective-C/C++), hem Android için (Java), hem Web Sitesi için (JavaScript, HTML5) OYUN kodlamak istediği zaman sirket 3 farklı dil bilen 3 kocaman geliştirici ordusu tutmak zorunda kalıyordu. Zamanında ActionScript programcısı olan Haxe'in kurucusu bu sorunu kökten çözdü. 

Haxe'i indirdiniz. Bir kod (Oyun veya Yazılım) yazdınız. Sonra Haxe'in siyah kutusuna "Derle!" dediniz ve karşınıza şu mükemmel seçenek menüsü çıktı: *"Neye Çevireyim Sahip?"* Eğer `Web` seçerseniz, yazdığınız kodu saniyesinde %100 Native **JavaScript Code Bloklarına** dönüştürür. `Desktop` derseniz **C++'ın** korkunç pointer dosyalarına çevirir! `Android` derseniz **Java** sınıfları basar. Bir Kez Yaz -> Her Dilde Native Yayınla!

**Ne İşe Yarar?**
* **Çoklu Platform (Cross-Platform) Oyun Motorları:** Bağımsız (Indie) oyun geliştiricilerin kalesi olan *Dead Cells*, *Northgard* veya *Paper Please* gibi efsanevi milyonlar satan oyunlar Haxe diliyle yazılmıştır. (Heaps.io veya OpenFL oyun motoru kullanılarak).
* **Bir Kodu Asla İki Kere Yazmama (Code Re-use):** Büyük bir şirketin arka yüzündeki (Server) veritabanı kodları ile, Cep telefonundaki (Client/App) hesaplama kodlarını aynı dil (Haxe) ile yazar; Haxe Server için `PHP/NodeJS` kodu üretirken, Client için Java/C++ çıktısı oluşturur. Proje asla çelişmez ve bug barındırmaz.

## Dilin Mantığı ve Kod Yapısı
Tam bir ActionScript ve ECMAScript (Yeni Nesil Java/C# melezleri) kopyasıdır. Tamamen Katı Tiplidir (Strict-Type) `var isim:String`. Nesne Yönelimli Programlamanın kurallarına DörtDörtlük uyar (Sınıflar, Miras, Interface, Generics). Neden Pürüzsüz derecede kurallı OOP kullanır? Çünkü ancak kurala tam binen ve hafızası şaşmayan bir DİLİ alıp, hiç hata yapmadan C++ gibi ölümcül sistemlere C-Kodu olarak kopyalayabilir (Transpile) edersiniz.

Ayrıca dünyadaki en kusursuz ve en büyük "Makro (Macros)" yeteneğine sahip betiklerdendir. Derlenirken (Örneğin C++'a çevrilmeye başlandığı an), siz "Benim yerime veritabanı sınıfını otomatik kopyala oluştur sonrada C++'a çevir" emri verirsiniz ve işi yapar.

**Örnek İşleyiş (Sembolik Olarak):**
Siz Haxe'de yazarsınız: `trace("Hello World");`
Eğer hedef JavaScript ise çıkar Çıktı(Derleyici): `console.log("Hello World");`
Eğer hedef C++ ise çıkar Çıktı(Derleyici): `std::cout << "Hello World" << std::endl;`
Eğer hedef Python ise çıkar Çıktı: `print("Hello World")`

### Örnek Bir Haxe Kodu: Tek Kodla Bütün Bilgisayar Ekosistemine Hükmetmek
Bir oyunun Karakter sınıfının Haxe formatında yazımı. Bu kod, Windows'a `.exe`, tarayıcıya `.js` ve telefona `.apk` olarak dökülürken Haxe'in o pürüzsüz "Ara Yüz / Meta Dil" sentaksı:

```haxe
// Haxe dili C-bazli (Java/JS/C#) yorum satirlari (//) ve bloklari kullanir

// PAKET / SINIF TANIMLAMA
// Dosyanin ana kontrol Blogu (OOP mimarisi sarttir, dümdüz script yazamazsınız)
class Kahraman {
    
    // TIPI KATI (STRICT TYPED) Değişken Tanimlamalari "isim : Tip" 
    public var isim:String;
    public var canPuani:Int;
    public var hasarGucu:Float;

    // OTOMATİK NESNE OLUŞTURUCU (Constructor - Javadaki gibi Sınıf adıyla degil new kelimesiyle)
    public function new(p_isim:String, p_can:Int) {
        this.isim = p_isim;
        this.canPuani = p_can;
        this.hasarGucu = 50.5;
    }

    // SINIFIN (CLASS) IC FONKSIYONU
    public function Saldiri_Yap():Void { // :Void (Geri donusu yok demektir)
        
        // Haxe'in Ekrana/Konsola Yazi Basma Mucizesi (Translate olan komut)
        // Her platformda "trace" O platformun print'ine, console.log'una dönüşür!
        trace(this.isim + " kilicini salladi ve " + this.hasarGucu + " hasar verdi!");
        
        // Klasik C / Java döngü yapısı tıkır tıkır işler:
        for (i in 0...3) { 
            // 0...3 (Sonda 3 Nokta): Haxe'e Ozel Harika Sirali Iterator (0, 1, 2 ceker)
            trace("Art arda Kiliç Darbesi: " + i); 
        }
    }
}

// PROGRAMIN CALISMA MOTORU (Mecburi Statik Main Fonskiyon)
class Main {
    static public function main():Void {
        
        // Objeyi yaratiyoruz. Eger platform JS ise var a = new yazar, C++ ise *a pointer diler!
        var basrol:Kahraman = new Kahraman("Arthur", 100);
        
        // Objeye Sinyal(Fonksiyon yolla).
        basrol.Saldiri_Yap();
        
        // Platform Makrolari (SADECE C++ Ise sunu derle, HTML ise Oteki kütüphaneyi Çek! diye şizofrenik bir yetenegi vardiR):
        #if js
            trace("Oyun Su an Tarayicida Calisiyor, Dom'a(Grafiklere) Erisim saglandi!");
        #elseif cpp
            trace("Oyun Steam C++ Formatinda! OpenGL grafik Motorunu Calistir.");
        #elseif java
            trace("Oyun Android Telefon icin hazir JVM Motorunu kullan.");
        #end
    }
}
```

Bu program bir Meta-Mimcidir. Kodcular Haxe IDE'sinde (Visual Studio Code vb) çalışır, altta yatan sistem ne istiyorsa o "Dilde" anında pürüzsüz üretim yapar.

## Kimler Kullanır?
* C/C++ karmaşası çekmeden Xbox, PlayStation, Nintendo Switch, iPhone ve Android için aynı anda (tek tuşla) Native **2D/3D Oyunlar çıkartan Indie(Bağımsız) Oyun Stüdyoları**. Oyun programlama tarihinin dev gizli sırlarındandır.
* TiVo gibi gömülü arayüz yazılımı yapan firmalar: "Yazılımı donanımın ne olduğundan(Linux mu? Windows mu? ARM Mı?) bağımsız olarak kurgulayıp, donanımı değiştirdiğimizde kodu yeniden yazmamak için evrensel çevirmen!" diyen mimar ekipler.
* (Not: Eskiden Javascript yerine tarayıcı için kodlayanlar varken son 10 yılda Javascript'in dev tekelleşmesi(TS vb) ile Haxe artık çoğunlukla Oyun ve Gömülü Performans tarafında varlık göstermektedir).
