# Rust

## Özet
Rust, Mozilla tarafından geliştirilen, C ve C++ dillerinin kanayan yarası olan "Bellek Güvenliği" (Memory Safety) sorununu, tamamen kendine has "Sahiplik" (Ownership) modeli ile çözen, modern sistem programlama dünyasının son yıllardaki tartışmasız en büyük yıldızıdır.

## Nedir ve Ne İşe Yarar?
Yıllarca sistem dillerinde kod yazmak demek, manuel olarak RAM tahsis etmek (`malloc`) ve sonra onu elle serbest bırakmaktı (`free`). Eğer programcı küçük bir hata yaparsa ya güvenlik açığı oluşur ya da sistem kitlenirdi. Diğer yandan Java, C# gibi diller bu işi "Garbage Collector (Çöp Toplayıcı)" adlı koruma melekleriyle çözdü fakat bu da programlarda (Oyun, Donanım sürücüsü vb.) gecikmelere (lag) sebep oluyordu.

Rust, 2010'lu yıllarda muazzam bir devrimle geldi: **Çöp Toplayıcısı yok, manuel Memory Free yok, ancak Bellek Güvenliği %100**.

**Ne İşe Yarar?**
* **Çekirdek Düzeyi (Kernel) Yazılımları:** Yıllarca sadece C'nin tekelinde olan Linux çekirdeğine ve Windows'un çekirdek bileşenlerine (DirectWrite vb.) resmi olarak dahil edilen "ikinci" tek dildir. Microsoft ve Google şu an aktif olarak eski C++ kodlarını Rust'a geçirmektedir.
* **Korkusuz Eşzamanlılık (Fearless Concurrency):** Rust Derleyicisi dünyanın en otoriter derleyicisidir. İki farklı işlemin (Thread) aynı anda aynı klasöre veya RAM adresine veri yazmaya çalışıp sistemi çökerttiği o "Data Race" (Veri Yarışı) hatası yaşanmaz. Kodunuz derlenmişse, çökmeyecektir.
* **WebAssembly (Wasm):** İnternet tarayıcıları içinde inanılmaz hızlı çalışması gereken dev araçların Rust ile yazılıp WebAssembly formatında web sitelerinde (Figma gibi) yayınlanması güncel teknoloji standardıdır.

## Dilin Mantığı ve Kod Yapısı
Dildeki devrimin adı **Ownership (Sahiplik)** ve **Borrowing (Ödünç Alma)** kurallarıdır. Rust'ta her değişkenin sadece ve sadece tek bir "Sahibi" vardır. O değişkenin kullanım işi aynı kod bloğunda bittiği an, C veya C++'daki gibi sizin belleği *free* etmenizi beklemez, Rust derleyicisi anında o boş silme kodunu arkaplanda makine diline yapıştırır.

Derleyici o kadar katıdır ki, eğer bellekteki bir değeri aynı anda iki farklı fonksiyona "Değiştirilebilir (Mutable)" olarak gönderirseniz derleyici (Rustc) hata fırlatıp size bağırır ve "Bu kod asla güvenli değil, derlemiyorum!" der.

**Örnek İşleyiş (Sembolik Olarak):**
Elinizde okumanız için tek bir orijinal kitap var. Rust derleyicisi diyor ki: "Bu kitabı (RAM'deki veriyi) aynı anda istediğiniz kadar arkadaşınıza OKUTMAK için kopyasını (immutable referans) verebilirsiniz. Ancak kitabı sadece BİR KİŞİ üzerine notlar alarak DEĞİŞTİREBİLİR (mutable referans)." Aynı anda hem kopya dağıtıp hem de ana kitabı birine karalattırmanız yasaktır. Bu muazzam kural, binlerce güvenlik açığını yazılım doğmadan tarihe gömmüştür.

### Örnek Bir Rust Kodu: Sahiplik ve Güvenlik
Rust'ta String işlemlerinde sahip olunan verinin nasıl yer değiştirdiğini (ve eski sahibin iptal edildiğini) gösteren klasik bir "Hata (Uzatılan El)" örneği:

```rust
fn main() {
    // Heap (RAM'in dinamik kısmı) üzerinde bir String oluşturuyoruz
    // 's1' bu metnin SAHİBİDİR (Owner)
    let s1 = String::from("Sistem Programlama Guvenligi");

    // DİKKAT: C++ veya Java'da bu sadece s1'i s2'ye kopyalardı.
    // Lakin Rust'ta bu işlem "Move" (Taşıma) işlemidir. 
    // Sahiplik TAMAMEN s2'ye GEÇTİ. s1 artık ÖLÜ ve GEÇERSİZDİR.
    let s2 = s1; 

    // println!("eski s1 nerede? {}", s1); 
    // ^^^ EĞER ÜSTTEKİ SATIRIN BAŞINDAKİ // SLASHLARI SİLERSENİZ:
    // Derleyici bağırır: "s1'in değeri zaten s2'ye taşındı (moved value), s1'i kullanamazsın!"
    // İşte bu "Double Free" (Çift Serbest Bırakma) güvenlik açığını önler.

    // Doğru Kullanım (Ödünç Alma - Borrowing): "&" işareti.
    // Metnin asıl sahibi hala s2'dir, fonksiyon sadece ona "okumak için" uzaktan bakar.
    uzunluk_hesapla(&s2); 

    // s2'nin işi burada, parantez bittiğinde sonlanır (Out of Scope).
    // BİZ manuel bir şey yapmayız, Rust arka planda s2'yi otomatik yok eder (Memory Free).
}

// Parametre olarak '&' ile (Ödünç Alınmış - Borrowed) veri geliyor
fn uzunluk_hesapla(gelen_odunc_metin: &String) -> usize {
    println!("Metnimiz: '{}', Uzunluk: {}", gelen_odunc_metin, gelen_odunc_metin.len());
    return gelen_odunc_metin.len();
}
```

## Kimler Kullanır?
* Tüm büyük Teknoloji Şirketleri (Big Tech): Microsoft, Google, AWS, Meta ve Mozilla. Cloud altyapılarını sıfır hata ve süper hız ile yeniden yazan mimarlar.
* Linux ve Windows işletim sistemi çekirdek (kernel) mühendisleri ile gömülü donanım (embedded) güvenlik geliştiricileri.
* Kriptografi ve Blockchain ağı çekirdeklerini kodlayan ("Smart Contracts" dahil) uçtan uca altyapı mühendisleri.
