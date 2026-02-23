# Zig

## Özet
Zig, C dilinin yerini almayı hedefleyen, inanılmaz şeffaf, modern, mikroskobik boyutta hızlı ve "Makine kodunda gizli saklı hiçbir şeye izin vermeyen" yeni nesil (2016) açık kaynaklı sistem programlama dilidir.

## Nedir ve Ne İşe Yarar?
Geliştiricisi Andrew Kelley, C dilindeki tarihi kalıntılardan ve gizli makro sorunlarından nefret ettiği, ama Rust dilinin de öğrenilmesi son derece ağır ve komplike kurallara sahip olduğunu düşündüğü için Zig'i yarattı. 

Zig felsefesinde "Açıklık ve Şeffaflık" vardır: "Eğer kodunuzun bir satırında RAM istendiği (Allocation), bir veri akışı sağlandığı yazmıyorsa; arka planda *gizli* hiçbir makro, fonksiyon veya sihir çalışamaz." 

**Ne İşe Yarar?**
* **Gömülü Sistemler (Embedded):** İşletim sistemsiz ortamlar, mikrokontrolcüler ve minik elektronik beyinler için (C'nin aksine) sıfırdan "Standart Kütüphanesiz" (freestanding) devasa boyutta hızlı kodlar yazar.
* **C ile Kusursuz Uyumluluk:** Dünyadaki tüm C/C++ derleyicilerinin yerini alabilen bir "Çapraz Derleyici (Cross-Compiler)" harikasıdır. Zig kullanarak on yıllanmış C kodu kütüphanelerini zerre değişiklik yapmadan programınıza anında import edebilirsiniz.
* **Oyun Geliştirme:** Makineye tam erişim ve sıfır gecikme sayesinde modern indie (ve büyük ölçekli) motor kodlamalarında, makrolar içinde boğulmadan geliştirme yapma imkanı sunar.

## Dilin Mantığı ve Kod Yapısı
Rust'ta derleyicinin koruma melekliğine güvenirsiniz; Zig'de ise her şey sizin kontrolünüzdedir ama C'deki gibi kuralsız değildir. Hafıza isteme işlemi (Allocation) asla yerleşik bir dil özelliğinin arkasına gizlenmez.

Örneğin C dilinde veya bir çok yüksek seviye dilde bir metin dizisini birleştirirken (`a + b`) arka planda dil sizden habersiz İşletim Sistemine gidip RAM talep edebilir. Zig'de bunu yapamazsınız! Zig'de eğer bir işlem RAM (Heap) istiyorsa, o fonksiyona dışarıdan bizzat bir `Allocator (Tahsis Edici)` değişkeni parametre olarak göndermek **zorundasınız**. Yazılımcı saniyesi saniyesine nerede, ne kadar bayt RAM tüketildiğini görmek mecburiyetindedir.

**Örnek İşleyiş (Sembolik Olarak):**
Hata yönetimi mucizevidir. Klasik C dillerinde bir fonksiyon başarısız olursa -1, -2 gibi saçma sayılar (Hata Kodları) döndürür ve yazılımcı if-else içinde boğulur. Zig'de bir fonksiyon eğer hata verebilecekse bunu belirtir (Başına `!` koyulur). Yazılımcı `catch` ile bunu anında temizce yakalamak zorundadır.

### Örnek Bir Zig Kodu: Açık Bellek Yönetimi (Allocator) ve Hatalar
Zig'de sadece belleğin değil, belleği "ne tarzda" tahsis edeceğimizin bile manuel olarak fonksiyona açıkça yedirildiğini (Örn: Standart Page Allocator kullanımı) ve hataların zarafetini görelim:

```zig
const std = @import("std");

// "!" işareti, bu fonksiyonun başarılı olursa 'void' (hiçbir şey) döndüreceğini, 
// ancak sorun çıkarsa hata (Error) fırlatma İHTİMALİ olduğunu derleyiciye söyler.
pub fn main() !void {
    
    // ZIG FARKI: RAM istemek otomatik bir şey değildir.
    // Fonksiyonlara bizzat bizim "Al kardeşim belleği bu sistemden iste" diye Allocator vermemiz lazım.
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator(); 
    
    // RAM yığınından (Heap) 100 sayılık bir dizi (array) için açıkça yer istiyoruz.
    // "try" komutu Zig'in hata denetimidir. Eğer RAM dolmuşsa anında 'catch' edip çökmeyi önler.
    var numbers = try allocator.alloc(u32, 100);
    
    // İŞTE ZİG ERDEMİ: Bellek silme (free) işlemini yazıp "defer" kelimesiyle işaretliyoruz.
    // 'defer', bu fonksiyon ne zaman sona ererse (ne kadar kod olursa olsun),
    // bu silme işlemini fonksiyonun en sonuna otomatik saklayıp çalıştıran sihirli bir emniyet subabıdır.
    defer allocator.free(numbers);
    
    // Diziye değerleri manuel atamalar
    var i: u32 = 0;
    while (i < 100) : (i += 1) {
        numbers[i] = i * 2; // Çift sayıları atıyoruz
    }

    // Konsol üzerine çıktı basma işi (! ünlem yine IO hatası olabileceğini simgeler)
    std.debug.print("10. Elemanın Değeri: {d}\n", .{numbers[10]});
}
```

Bu `defer` yapısı C'nin en büyük problemi olan "if-else içine girip yanlışlıkla `free` etmeyi unutunca olan bellek sızıntılarını" mükemmel bir zarafetle çözer.

## Kimler Kullanır?
* C dilinin aşırı yaşlı ve makrolarla (Define C-Preprocessor) darmadağın olmuş altyapısından bıkan, ancak modern bir sistemsel şeffaflık arayan güncel sistem derleyicileri ile araçlarını (örneğin Bun javascript çalışma zamanının yaratıcıları) tasarlayan mimarlar.
* Rust'ın devasa güvenlik öğrenme eğrisinden (Learning curve) kaçıp, doğrudan "ben ne yazdığımı donanımda görmek istiyorum" diyen yeni nesil saf donanım ve işletim sistemi yazarları.
