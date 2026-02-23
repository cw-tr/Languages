# Chisel

## Özet
Chisel (Constructing Hardware In a Scala Embedded Language); 2012 yılında Berkeley Üniversitesi'ndeki araştırmacılar (RISC-V mimarisinin yaratıcıları) tarafından geliştirilen, Verilog/VHDL'in ilkel ve manuel eziyetinden kurtulmak için **modern Scala dili üzerine (JVM) inşa edilmiş**, parametrik ve yeni nesil yüksek seviyeli Donanım Üretme (Hardware Construction) dilidir.

## Nedir ve Ne İşe Yarar?
Donanım tasarımı dünyası (Verilog/VHDL) 30 yıldır bir çeşit duraklama içindeydi. Apple M1 işlemcisi veya NVIDIA GPU'su gibi devasa karmaşık zekadaki sistemler için Verilog kodlamak, binlerce kabloyu (Wire) tek tek elle bağlamaya benzetiliyordu ve C/C++'ın hızlandıran otomasyonları (Generics) burada yoktu.

Chisel, bir "Donanım Tanımlama Dili (HDL)" DEĞİLDİR. Chisel, **Scala kullanarak Donanım Jeneratörü Üreten bir Kütüphanedir**. Siz çok az kodla, modern Java/Scala yazılım vizyonunu (For döngüleri, Fonksiyonel map/filter özellikleri, Nesne Kalıtımı) kullanarak bir kod yazarsınız. Chisel'e "Derle" dediğiniz an, Chisel bu koddan saniyeler içinde on binlerce satırlık %100 kusursuz saf "Verilog" kodu oluşturur ve size verir. Fabrikaya yine Verilog dosyası gider, ama siz onu elle yazma eziyetinden kurtulursunuz.

**Ne İşe Yarar?**
* **RISC-V ve Yeni Nesil SoC Üretimi:** Bugün Çin'in, Avrupa'nın ve açık kaynak dünyanın en hızlı gelişen mikroişlemci mimarisi olan RISC-V çekirdekleri tamamen Chisel ile yazılmıştır (Örn: Rocket Chip).
* **Donanım Parametrizasyonu (Generators):** Verilog'da "Sadece 8 bitlik olanı 16 bite çevir" demek o devreyi baştan yazmaktır. Chisel'da sadece 1 satırda `(Width = 16)` parametresini verip, döngüyle anında saniyede yepyeni bir donanım modeli YARATIRSINIZ.

## Dilin Mantığı ve Kod Yapısı
Tamamen JVM tabanlı SCALA dilidir! Yazılımcılara ait "Nesne Yönelimli Programlama (OOP)" ve "Fonksiyonel Programlama" güçleri, Chisel'da donanım kablolarına aktarılır. 

Kod içerisinde modüller (Devre Kutuları) yaratmak için Scala Sınıfları `class` yaratılır. Tasarımın sinyallerini (Giriş / Çıkış) `Bundle` adı verilen çantalarda gruplarız. Çok güçlü `val` (Immutability - Değiştirilemez) kurallarıyla donanım hataları daha Scala kodlanırken kilitlenir kalır (Tip güvenliği / Type Safety). 

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin Verilog'da 250 adet işlemci çekirdeğini birbirine bağlayan ağ kabloları için 2000 satır manuel IF/ELSE döşersiniz. Chisel'da `for(i <- 0 to 250)` diye standart C++/Scala yazılım döngüsü açarsınız, bu döngü hardware devresini (Makine basımı anında değil, Chisel kendi içindeyken) otomatik olarak üretip, dev Verilog dosyasını 1 saniyede size tükürür.

### Örnek Bir Chisel Kodu: Modern Scala Zarafetiyle Çip (MUX) Tasarımı
İki giriş sinyalinden hangisinin çıkışa (Output) yansıyacağını kararlaştıran basit bir "Multiplexer (Seçici - MUX)" devresinin Verilog hamallığı olmadan Chisel/Scala ile şıkça yazımı:

```scala
// Klasik C++/Java/Scala cift slash '//' Yorum Satırları

// JVM uzerinden Chisel Donanim Kutuphanelerini Iceri aktar (Import)
import chisel3._

// 1. ADIM: Donanım Sınıfını (Module) Scala Sınıfı olarak Tanimlamak
class MuxDevresi extends Module {
    
    // 2. ADIM: Donanımın IO (Input / Output) Pinlerini (Kablolarını) "Bundle(Kutu)" icine Mühürlemek
    val io = IO(new Bundle {
        // İki adet 1 Bitlik (UInt) "Girilecek Sinyal" Pini:
        val sinyal_A = Input(UInt(1.W)) 
        val sinyal_B = Input(UInt(1.W))
        // 1 bitlik "Secici Şalter" sinyali (0 ise A geçsin, 1 ise B geçsin):
        val salter   = Input(UInt(1.W))
        // 1 bitlik "Donanımdan Çıkış":
        val donanim_cikisi  = Output(UInt(1.W))
    })

    // 3. ADIM: Donanımın Lojik (Kapasitif) Davranisi
    // Scala'nin mucize "when / .otherwise" fonksiyonlarıyla, C kodu yazar gibi DONANIM CIZILIYOR!
    
    when(io.salter === 0.U) { 
        // Salter 0.U (UInt 0 Biti) ise Çıkış Pini A'ya direkt LEHİMLENSİN ( := isareti kablo lehimlemedir)
        io.donanim_cikisi := io.sinyal_A 
    } .otherwise {
        // Değilse Çıkış Pini B'ye Lehimlensin!
        io.donanim_cikisi := io.sinyal_B
    }
}

// BU KODU CALISTIRINCA NE OLUR? 
// Scala JVM motoru calisir, Bu class'i Okur ve fabrika için %100 donanım ciktisi(Verilog.v dosyasi) FIRLATIR!
```

Mühendisler donanımı artık krikolarla değil, modern yazılım algoritmasının o döngüleri ve OOP kalkanları arkasından devasa ölçekte tasarlamaktadırlar.

## Kimler Kullanır?
* Google'ın açık kaynaklı "TPU" (Tensor İşleme Çipleri / Yapay Zeka donanımı) tasarlayan mimar araştırma ekipleri. 
* RISC-V Vakfı (Foundation) standartlarında işlemci mimarisi geliştiren Akademisyenler (M.I.T / Berkeley Üniversitesi).
* "Agile Hardware (Çevik Çip Geliştirme)" manifestosunu takip eden, sürekli kodu değiştirip derleyen modern Silikon Vadisi Çip Start-up'ları (Örn: SiFive Donanım Devi).
