# Nim

## Özet
Nim, Python yazıyormuş gibi hissettiren ancak yazdığınız o zarif kodu devasa bir hızla C veya C++ diline derleyerek saf donanım gücüyle çalışmasını sağlayan (Transpiler) modern, eşsiz ve statik tipli bir sistem programlama dilidir.

## Nedir ve Ne İşe Yarar?
Geliştiricilerin yıllar süren bir hayali vardı: "Öyle bir dil olsun ki sözdizimi (syntax) Python kadar temiz ve okunması keyifli olsun, ama yazdığım bu program C veya Rust kadar şimşek hızında ve hatasız çalışsın". İşte Nim bu felsefeyle Andreas Rumpf tarafından yaratılmıştır.

Sırrı basittir; yazdığınız kodları doğrudan "1 ve 0" makine diline çevirmez. Bunun yerine (arka planda) sizin Nim kodunuzu devasa ve mükemmel optimize edilmiş bir C koduna (.c dosyasına) çevirir ve o C kodunu standart sistem derleyicinizden geçirir.

**Ne İşe Yarar?**
* **Performans ve Şıklık Hibridi:** Karmaşık oyun sunucuları, ağır backend yazılımları veya devasa veri madenciliği gibi Python'un felaket yavaş kalacağı noktalarda, Nim kodlamanın hızıyla C dilinin makine hızını (ve bellek mikroyönetimini) harmanlar.
* **Makrolar ve Metaprogramlama:** Dünyadaki en güçlü Meta-programlama (kod yazan kod üretme) özelliklerinden birine sahiptir. Dili kendi amaçlarınıza göre, yeni kurallar atayarak (Makrolar sayesinde) bükebilir ve yeni bir Syntax icat edebilirsiniz.
* **Her Platforma (Çapraz Derleme):** C koduna çevrilen bir şey, evrendeki her donanımda çalışabilir mantığıyla her yerde (Windows, Linux, IoT cihazlar, hatta WebAssembly için JS'e bile çevrilir) yerel (native) gücüyle yer bulur.

## Dilin Mantığı ve Kod Yapısı
Girinti tabanlı (Indentation-based) bir sözdizimi kullanır, yani tıpkı Python gibi süslü parantezler `{ }` veya noktalı virgüllerle (`;`) sizi darlamaz. Kod blokları içeriden başlar.

Ancak en büyük farkı **Tiplerde** yatar. Python yazarken `a = 5` ve `a = "Merhaba"` demek serbesttir (Hata anı Runtime'da gelir). Nim, o temiz kaportanın altında "Katı Tipli (Strongly & Statically Typed)" bir zırhla kaplıdır. Kod derlenirken (Compile-time) tiplere ölümüne dikkat eder, bu da C hızındaki optimizasyonunun anahtarıdır. Bellek yönetimi son derece lükstür (Garbage Collected veya ARC koruması tercih edilebilir).

**Örnek İşleyiş (Sembolik Olarak):**
Ekrana 10 defa metin basan bir komutu Python'la yazmanızla Nim'le yazmanız gözle görülür bir fark taşımaz. Sadece Nim arkaplanda `var integer` diye değişken tutar ve bunu C döngüsüne sokuşturarak size o .exe'yi saniyeler içinde verir. İşletim sistemi bu programı saf bir C donanım kodu sanır.

### Örnek Bir Nim Kodu: Hızlı, Temiz ve Tipe Bağlı
Görünümü Python ancak performansı ve alt katmanı C olan örnek Nim kod yapısına bir bakış:

```nim
# Nim dilinde '#' etiketi yorum satırıdır tıpkı Python'daki gibi 

# Bir değişken (variable) oluşturuyoruz. 
# Değişkenin tipleri Nim compiler tarafından "Tip Çıkarımı" (Type inference) hilesiyle anlaşılır.
var kelime = "Sistem Dili"   # Derleyici otomatikman bunun 'string' olduğuna karar verdi.
var sayi: int = 42           # İsterseniz de tipini ('int') elle dayatırsınız.

# kelime = 15 
# Üstteki (açıklanan) satır Python'da çalışırdı. Nim'de DERLENMEZ. "Zaten string'di, integer yapamazsın" der!

# Bir 'procedure' (Nim'de fonksiyonlar böyle geçer) tanımlayalım:
# İki sayıyı alır, integer döndürür. Süslü parantez yok, blok 'girinti' (indent) ile belli olur.
proc toplaVeYazdir(a, b: int): int =
  result = a + b    # 'result' kelimesi Nim'de ön tanımlı bir dönecek değer atamasıdır, Return yazmaya gerek yok!
  echo "Toplamlari: ", result  # "echo" komutu print işlevini görür

# Oluşturduğumuz saf fonksiyonu çağırıyoruz (Yine C hızıyla bellekte uyanacaklar)
let hesaplanan = toplaVeYazdir(50, 20) 
```
Yukarıdaki tertemiz ve süslü parantezsiz (Okunabilirliği maksimize edilmiş) kod, Nim derleyicisi tarafından anında GCC'ye C formatıyla havale edilir ve harika bir `exe` verilir.

## Kimler Kullanır?
* Python dilini çok seven ama ağır matematiksel oyun motorları veya blockchain işlemleri geliştirirken (Örn: Status ağı "Nimbus" projesi) sürat yüzünden Python'u (veya dildeki Global Interpreter Lock sınırlarını) terk etmek zorunda kalanlar.
* Derleyicileri seven açık kaynak bilgisayar korsanları. Yazdıkları makrolarla (dilin kalbine girmek) kendi domain-spesifik dillerini saniyeler içinde üreterek otomasyon yapan yazılımcılar. 
* Kötü yazılmış (legacy) C kütüphanelerini sarmak ve tertemiz (Nim) syntax'ı üzerinden çağırıp C gücünü sağmak isteyen arayüz/backend programcıları.
