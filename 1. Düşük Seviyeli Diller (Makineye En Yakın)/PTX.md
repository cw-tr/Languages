# PTX (Parallel Thread Execution)

## Özet
PTX (Parallel Thread Execution); donanım devi **NVIDIA** tarafından GPU'ların (Ekran Kartlarının) içine Makine Öğrenimi (Yapay Zeka) ve dev matrix hesaplamaları yaptırabilmek için özel olarak geliştirilmiş, LLVM IR'e benzeyen ancak İşlemci (CPU) yerine muazzam **Grafik İşlemcisine (CUDA-GPU)** ait olan "Ara Makine" (Sözde-Assembly / Pseudo-Assembly) dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda ekran kartları oyundaki poligonların/grafiklerin rengini değiştiren aptal monitör cihazlarıydı. 2006'larda NVIDIA "CUDA" teknolojisini çıkardığında "Biz oyun ekranı piksellerini hesaplamak yerine, buradaki 5000 adet renk hesaplama Çekirdeğini dev matris matematik problemlerine sokarsak (Yapay Zeka Ağları) CPU'ları milyar kez ezen bir hız buluruz" dedi (GPGPU: Genel Amaçlı GPU Kullanımı).

İşte PTX, yazılımcının C++ ile veya Python ile yazdığı bu kodları alıp, NVIDIA firmasının kapalı kapılar arkasındaki milyarlarca dolarlık Gerçek Hardware Assembly'sine (SASS - Shader Assembly) çevirmeden saniyeler önce "ortaya açılan" **Ara Jenerasyon Dilidir**. Çünkü her 2 telefonda veya PC'de farklı (eski veya çok son teknoloji) bir NVIDIA çipi olabilir. PTX, "Her NVIDIA kartında çalışabilecek Evrensel Ara Köprü Cümlesi"dir; Cihaza (örnek GTX 1080) iner inmez, cihaz kendi kapasitesine ve çip teknolojisine göre onu en son saniye yutarak kendi saf donanım assemblysine (SASS) derler.

**Ne İşe Yarar?**
* **Yapay Zeka Çekirdeği Pompası:** ChatGPT gibi muazzam Large Language Model'ler ağzından çıkan (Generate edilen) o devasa Token (Sözcük) yapısını arka planda eğitirken Milyarlarca veriyi CPU'ya değil Paralel çalışsın diye GPU'ya iter. GPU, bunu doğrudan "PTX ve SASS" formatlarıyla emer.
* **Geriye Dönük Uyumluluk (Kalkan):** Eğer yazdığınız Yapay Zeka botu çok düşük segment (10 yıllık) NVIDIA Quadro kartıyla donatılmış bir hastanenin sisteminde çalışacaksa, PTX teknolojisi C koduyla hastane yazılımını "eski cihaza uygun" derlemekle (JIT Compile) şaha kalkar ve sizin kodlarınızı cihazın o günkü kapasitesine göre kırpar. Evrenseldir (Nvidia ekosisteminde).

## Dilin Mantığı ve Kod Yapısı
Tip olarak LLVM IR'den bile daha çok klasik Assembly'yi anımsatır lakin çok modern eklemeleri vardır (Vektör destekleridir). 

Normal Array veya Veri yığını yerine ekran kartları "Eşzamanlı Parça Akışına (SIMT - Single Instruction Multiple Thread)" dayanır. Yani siz tek bir `ADD (TOPLA)` emri yazarsınız PTX dilinde. Arka planda Ekran Kartının içinde yer alan 10,000 farklı minik işci (Grid / Core Thread), hepsi aynı anda ellerindeki farklı elmayı (Memory Blokunu) yanındakiyle toplayıp çözer (`Warp` Mimarisi). 

Değişkenlerin sonlarına `.f32` (32 bit Float), `.s32` (Signed Int) veya donanıma özgü adres boşlukları (Örn: Ekran Kartı Ana Rami mi? Kendi iç Küçük Önbelleği mi? -> `.global`, `.shared`) gibi aşırı spesifik ekran kartı ayar ekleri koyarak emir verirsiniz. 

**Örnek İşleyiş (Sembolik Olarak):**
Siz C/CUDA'da iki satır matris eklersiniz. PTX kodu: GPU'nun "Bir Global hafızadaki" bloğu (.global) "Kendi Çekirgeklerindeki Odaya (.shared)" çekip, orada Vektör (.v4) matematiği atıp Çarpıp tekrar anakarta fırlatmasıdır. Bütün bunlar tekil 0/1 Makine kodundan bir satır önce (Driver taraflı) oluşur.

### Örnek Bir PTX Kodu: İki Sayıyı Toplayan Ekran Kartı İşlemi
NVIDIA Driver (Sürücüsü) tarafından havada okunup anında Çip'e (Silicona) şırınga edilen o meşhur CUDA-Ara Kod mimarisinin en okunabilir hali:

```ptx
// PTX kodunda cift taksim (//) yorum anlamindadir.

// Ekrana/Kernel'a (GPU'nun ta kalbine) Açık '.visible' olan '.entry' ana Metodu / Baslangici
// Parametrelerini .global (Ana VRAM Uzerisinden) ve yıldıza pointer/adres isaret ederek cekiyor!
.visible .entry cekirdek_toplama (
  .param .u64 p_sayi1_adresi,
  .param .u64 p_sayi2_adresi,
  .param .u64 p_sonuc_adresi
)
{
  // İLK İŞ C++'taki gibi Registers (Küçük Kasalar / Çekmeceler) Tanımlamak:
  // '%rd' serisi 64 bi Pointerlar için, '%f' serisi ondalikli(Float-32) matematik hesaplari için.
  .reg .u64   %rd<4>; 
  .reg .f32   %f<4>;  

  // C Programından Bize Parametre (Param) denen adresleri Çek(Load, ld) ve 64-bit(%rd) Pointerlere aktar:
  ld.param.u64    %rd1, [p_sayi1_adresi];
  ld.param.u64    %rd2, [p_sayi2_adresi];
  ld.param.u64    %rd3, [p_sonuc_adresi];

  // EKRAN KARTI YAVAŞ VERİTABANI OLAN VRAM'DEN VERİYİ KASAYA (f32) ÇEKER(Load):
  // "Al kardesim bu Ram adresinden(rd1) gelen veriyi (f1) kasasına float .f32 olarak işle"
  ld.global.f32   %f1, [%rd1];
  ld.global.f32   %f2, [%rd2];

  // MATEMATİK ZAMANI MUCİZESİ ("CUDA CORE" KÜKREMESİ):
  // ".add" komutuyla İki kutuyu f32 hassasiyetinde toplayip, ucarak yepyeni '%f3' kasasina aktar
  add.f32         %f3, %f1, %f2;

  // SONUCU EKRAN KARTININ RAMINE (VRAM) GERİ YAZAR (Store - st):
  // Toplam sonucu tutan %f3'u al, Ram degeri olan rd3 ibresinin(hedefi) Ustune civi gibi cak
  st.global.f32   [%rd3], %f3;

  // Islem bitti, CUDA cekirdegi donup bir sonraki talimata veya uykuya gecebilir (ret/Return)
  ret;
}
```
Bu tek emir parçacığı (Add.f32), arka planda milyarlarca saniyede milyonlarca farklı RAM bloğuna çarptığı an evrenin en ölümcül Yapay Zeka Hızlandırıcısı (Ekran Kartı GPU'su) ortaya çıkar.

## Kimler Kullanır?
* NVIDIA'nın ana şirket merkezindeki (Santa Clara) CUDA Ekosistem/Mühendis takımları (Derleyici Yazarları). 
* Yapay Zeka Modellerini (OpenAI veya Meta şirketleri) programlarken derin öğrenmenin Python kodlarından, Pytorch kütüphanesine inerken o matematiksel dev hesapların C'ye düşmeden 1 saniye önce "Ben bu matrisi söküp NVIDIA cihazın damarlarına (Assembly'ye) saf güçle nasıl yollarım?" diyen Makine Optimizasyon Sihirbazları.
* Ufak çaplı PTX kodlaması yaparak kriptopara madenciliğinde GPU mimarisinden maksimum verimi çekip sıfır sızıntı ile matematik arayan performans (Kripto-Miner) çılgınları. 
