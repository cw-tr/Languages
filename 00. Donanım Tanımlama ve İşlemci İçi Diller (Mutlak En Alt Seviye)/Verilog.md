# Verilog

## Özet
Verilog, dijital elektronik devrelerini modellemek, donanımsal simüle etmek ve fiziksel üretimini sağlamak için kullanılan, VHDL'in en büyük alternatifi olan diğer bir popüler Donanım Tanımlama Dilidir (HDL). Kökleri C dilinden geldiği için esnek, modern ve yazılımcıların daha kolay adapte olabildiği pratik bir yapısı vardır.

## Nedir ve Ne İşe Yarar?
Tıpkı rakibi VHDL gibi Verilog da bir yazılım geliştirme aracı değil, donanım mimarisinin tasarım kağıdıdır. En temel transistör seviyesinden, karmaşık çoklu çekirdeklere (mikroişlemcilere) kadar elektronik çiplerin (IC, ASIC, FPGA) yapısını ve veri yolu işleyişini belirler. Günümüzde güçlendirilmiş versiyonu "SystemVerilog" ismiyle anılmaktadır.

**Ne İşe Yarar?**
* **Çip (IC) Tasarımı:** Kullandığımız akıllı telefonların, bilgisayarların işlemcilerinin, bellek yongalarının veya ağ kontrolcülerinde akan elektronik devre mantığının sıfırdan oluşturulmasını sağlar.
* **Test ve Simülasyon:** "Testbench" adı verilen modüller aracılığıyla; tasarım halindeki çipe sanal ortamda binlerce farklı voltaj senaryosu (elektrik sinyali) gönderilerek, devrenin üretime gitmeden önce çökme yapıp yapmayacağı (Verification) test edilir. 
* **Pragmatik Çözümler:** Aşırı sıkı kuralcı VHDL diline kıyasla, hataları daha hızlı tolere eder, daha az kod yazmayı (boilerplate eksikliği) sağlayarak tasarıma hız katar.

## Dilin Mantığı ve Kod Yapısı
Verilog, çip üzerindeki parçaları `module` (modül) adı verilen birbirinden izole kutucuklar olarak hayal eder. Bir bilgisayarın anakartında nasıl ayrı ayrı lehimlenmiş çipler varsa, Verilog'da da kendi iç mantığına sahip olan bu modüller birbirlerine fiziksel olarak `wire` (kablo) tipi değişkenlerle bağlanır.

VHDL eşzamanlılığa ve tiplere karşı aşırı katı iken, C diline çok benzeyen sözdizimi sayesinde Verilog yazılımcılara donanımı çok daha esnek modelleme imkânı sunar.

**Örnek İşleyiş (Sembolik Olarak):**
Yine standart iki girişli bir "VE" (AND) kapısı devresi hayal edelim. Verilog bu donanım ünitesini kapsüllenmiş bir modül kutusu içine alır. Kutuya 2 dış bağlantı pini girdiğini, içerideki bir kablonun 2 kabloyu AND kapısı mantığıyla birleştirdiğini (lehimlediğini) beyan eder ve tek bir kabloyu dış pinden çıkarır.

### Örnek Bir Verilog Kodu: Basit AND Kapısı

```verilog
// 1. ADIM: Donanım parçasını, yani "Modülü" ve elektrik pinlerini (giriş/çıkış) tanımlıyoruz.
module AND_kapisi (
    input wire A,  // A girişi (Fiziksel bir kablo modeli: wire)
    input wire B,  // B girişi
    output wire Y  // Y çıkış noktası
);

    // 2. ADIM: Devrenin donanımsal davranışını fiziksel olarak bağlıyoruz.
    // 'assign' emri, A ve B'deki voltaj değişikliklerini anında Y pinine ittirir. 
    // Süreklidir (Continuous Assignment), şalteri açtığınız an elektrik akar.
    assign Y = A & B;

endmodule
```
Buradaki `assign` anahtar kelimesi, donanımda eşzamanlılığı temsil eder. Yazılım programlarındaki `Y = A + B` matematiğinin aksine; Verilog'da `assign` yapıldığında, A veya B pinindeki elektrik düzeyi değiştiği o milisaniye içinde, saat darbeleriyle (clock pulse) birlikte Y değerine bu durum donanımsal yapısı gereği anında uygulanır.

## Kimler Kullanır?
* Silikon Vadisi ve küresel donanım devlerindeki (Apple, Nvidia, Qualcomm, Broadcom vb.) çip (IC) ve donanım tasarım mühendisleri.
* Dünyada endüstri standartlarında genel bir bölüşüm vardır: VHDL daha çok Avrupa ülkelerinde ve askeri havacılık projelerinde güvenliği öncelik aldığı için kullanılırken; Verilog daha ziyade Kuzey Amerika'da, özel sektörde ve son tüketici silikon teknolojilerinde hız ve esnekliği sayesinde tercih edilir.
* Sistem on Chip (SoC) tasarlayan mühendis takımları.
