# SystemVerilog

## Özet
SystemVerilog (SV); 2005 yılında IEEE tarafından standartlaştırılan, eski ve ilkel Verilog dilinin üzerine C++ ve Java'dan ilham alınarak muazzam Nesne Yönelimli Programlama (OOP) yetenekleri, rastgeleleştirme (Randomization) ve doğrulama (Verification) kalkanları inşa edilmiş dünyanın bir numaralı **Donanım Tanımlama ve Doğrulama Dili**dir (HDVL - Hardware Description and Verification Language).

## Nedir ve Ne İşe Yarar?
1995'lerde çıkan klasik Verilog, küçük çipler tasarlamak (RTL - Register Transfer Level) için harikaydı ancak Intel veya Apple gibi dev şirketler milyarlarca transistörlük bir İşlemciyi (CPU/SoC) sadece Verilog'un ilkel kablolarıyla tasarlarken korkunç güvenlik açıkları ve hatalar yaşıyordu. Bir çip (Silikon) basıldıktan sonra içindeki hatayı düzeltemezsiniz (Yazılım gibi Update atılamaz). 

SystemVerilog bu sorunu çözmek için icat edildi. C dilindeki Sınıf (Class), Struct, Enum gibi karmaşık Yapıları (Data Structures) Verilog'un içine entegre etti. 

**Ne İşe Yarar?**
* **Devasa Çip Doğrulaması (Verification):** Geliştirilen çiplerin, fabrikaya "Silikon basımına" gönderilmeden önce sanal ortamda milyarlarca farklı sinyal ihtimaline karşı %100 kusursuz test edilmesini sağlar. Buna sektörel olarak UVM (Universal Verification Methodology) denir ve tamamen SystemVerilog içindeki OOP kütüphanelerine dayanır.
* **Tasarım (Design RTL):** Klasik Verilog'daki "Bu kablo nerede kopuyordu?" belirsizliğini `logic` anahtar kelimesiyle çözer, devrenin (D-Flip Flop vb) yazımını çok daha güvenli hale getirir. 

## Dilin Mantığı ve Kod Yapısı
Gelenekselleşmiş Verilog'un üst kümesi (Superset) gibidir. Verilog'daki zayıf ve çok karışan `wire` (Kablo) ve `reg` (Yazmaç/Kutu) tiplerini çöpe atıp yerine **`logic`** adında C-vari her iki yöne de uygun tek tip bir sinyal zırhı getirmiştir.

Sanal çiplerde Assertions (İddia/Test Kesinlemesi) ve Constrained Randomization (Kısıtlı Rastgele Veri Üretimi) gibi kod bloklarına sahiptir. Siz donanıma "Bana 20'den büyük ama 50'den küçük, sonu çift sayıyla biten milyarlarca rastgele Test Sinyali (Packet) yolla" komutunu saniyeler içinde verirsiniz.

**Örnek İşleyiş (Sembolik Olarak):**
Java'da "Araba Sınıfı" oluşturur gibi, SystemVerilog'da "Ağ Sinyal Paketi Sınıfı (Network Packet Class)" yapıp, içine fonksiyonları (`task` ve `function`) koyarak C++ gibi çalışmasına rağmen arka planda bir Elektronik Devre Test makinesini çıldırmış gibi sınarsınız.

### Örnek Bir SystemVerilog Kodu: OOP ve Kısıtlı Rastgele Test
Çip doğrulama mühendislerinin (Design Verification Engineers), bir entegrenin belleğine yollanacak paketleri "Rastgele ama kısıtlı" olarak üretip Hardware ile koşturan kod yapısı:

```systemverilog
// SV'de C dilindeki gibi çift // veya /* yorum satırları geçerlidir.

// KLASİK VERILOGDA OLMAYAN, JAVA BENZERİ OOP (Sınıf) MIMARISI:
class AğPaketi;
    
    // "rand" anahtar kelimesi SV'nin MUCİZESİDİR. 
    // Derleyiciye "Bunun değerini ileride sen rastgele belirle" deriz!
    rand bit [7:0] adres;   // 8-bitlik Rastgele Değişken Adres Sinyali
    rand bit [7:0] veri;    // 8-bitlik Rastgele Veri
    bit [7:0] hata_kodu;    // Normal Degisken
    
    // Constraint (Kısıtlama Zırhı):
    // "Adresleri rastgele at ama HİÇBİR ZAMAN 0 ve 255 atama, bellek çökmesin" kuralı
    constraint gecerli_adresler { 
        adres > 0; 
        adres < 255; 
        
        // Veri daima çift sayı olsun (Matematiksel Mod):
        veri % 2 == 0; 
    }
    
    // Fonksiyon Tanımlama
    function void goster();
        $display("Adres: %0h | Veri: %0h", adres, veri); // %0h = Hexadecimal ekran çıktısı
    endfunction
    
endclass


// UYGULAMAYI / TESTİ BAŞLATMA ZAMANI (İşletim Bloğu)
module test_sistemi;

    // Sınıftan bir KOPYA (Object/Handle) yaratıyoruz
    AğPaketi paketim;

    initial begin
        // Nesneyi Hafızada yarat (Memory Allocation - C++ gibi)
        paketim = new();
        
        $display("=== SV Rastgele Çip Testi Başlıyor ===");
        
        // 5 kere devreyi farklı verilerle bombardımana tutan döngü:
        repeat(5) begin
            // Sihirli 'randomize()' metodu, Sınıftaki "rand" ve "constraint" kurallarına göre  
            // zar atıp veriyi anında mükemmel şekilde oluşturur:
            if (paketim.randomize()) begin
                paketim.goster(); // Doğru Değerleri Ekrana (Console) Bas
            end else begin
                $fatal("Hata: Rastgele veri üretilemedi! Çip Kilitlendi.");
            end
        end
        
        $finish; // Simülasyonu Mükemmelen Sonlandır
    end
endmodule
```

## Kimler Kullanır?
* Evrendeki bütün (Intel, AMD, NVIDIA, ARM) mikroçip Mimarları (Hardware Architects) ve Çip Tasarım / Doğrulama Mühendisleri (Verification Engineers / DV).
* Klasik Verilog kodlayan yaşlı mühendislerin yanına, "Çip tasarımını da C programlar gibi esnek programlayalım" diyen yeni nesil bilgisayar ve elektronik entegratörleri.
