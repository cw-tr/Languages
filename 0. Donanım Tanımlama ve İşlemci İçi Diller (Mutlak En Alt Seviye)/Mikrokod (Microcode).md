# Mikrokod (Microcode)

## Özet
Mikrokod, geleneksel yazılımın ve hatta makine dilinin de altında yer alan, doğrudan donanım bileşenlerine hükmeden en temel yönlendirme seviyesidir. Bir programlama dilinden ziyade "donanımın yazılımı" olarak tanımlanabilir.

## Mikrokod Nedir ve Ne İşe Yarar?
Mikrokod (Microcode), mikroişlemcilerin (CPU) içindeki Aritmetik Mantık Birimi (ALU), kayıt ediciler (registers) ve veri yolları (buses) gibi fiziksel donanım elemanlarının kapılarını elektronik düzeyde açıp kapatan bit dizileridir. 

Makine dili (Örneğin; x86 veya ARM Assembly kodlarının 0 ve 1'lere derlenmiş hali) işlemciye ulaştığında, modern karmaşık işlemciler bu talimatları tek hamlede işletmezler. Gelen bu makine dili talimatını, işlemcinin elektronik devrelerinin tam olarak ne yapacağını söyleyen daha küçük, basit **mikro-işlemlere (micro-operations veya μops)** bölerler. İşte bu mikro-işlemleri tanımlayan ve yöneten yapıya **Mikrokod** denir.

**Ne İşe Yarar?**
* **Karmaşık Komutları Basitleştirme:** Dev boyutlardaki makine dili talimatlarını işlemcinin iç donanımının anlayabileceği adım adım elektrik sinyallerine dönüştürür.
* **Donanımsal Hata Giderme (CPU Patching):** Üretilmiş bir işlemci üzerindeki donanımsal mantık hataları veya açıkları (Örn: Meltdown ve Spectre), mikrokod güncellemeleriyle "yazılımsal olarak" kapatılabilir. İşlemcinin o devreyi kullanma şeklini değiştirebilirsiniz.
* **Geriye Dönük Uyumluluk (Emülasyon):** İşlemci mikro-mimarisi değişse bile, mikrokod sayesinde eski nesil programlar ve komut setleri sorunsuzca çalıştırılmaya devam edilebilir.

## Dilin Mantığı ve Sinyal Yapısı
Mikrokod, alışkın olduğumuz tarzda (değişkenler, döngüler, fonksiyonlar içeren) genel amaçlı bir programlama dili değildir. Çok uzun bit uzunluklarına (örneğin 64-bit ile 128-bit veya daha geniş) sahip "kontrol kelimeleri"nden (control words) oluşur.

Her bir bit, işlemcinin içerisinde yatan fiziksel bir anahtara bağlıdır.

Mikrokod, bildiğimiz anlamda bir kodlama dilinden ziyade "Kontrol Sinyalleri" listesidir. 0 ve 1'lerden oluşan geniş bit dizilimleridir ve işlemcinin içerisindeki yolları fiziksel olarak açıp kapatırlar.

Örneğin temel bir işlemci mimarisinde (örneğin SAP-1 mimarisi) her donanım parçasının bir kontrol sinyali (basit bir bit) vardır:
* `Ep` (Enable Program Counter): Program sayacının değerini Ana Veri Yoluna (Bus) aktar.
* `Lm` (Load Memory Address): Veri yolundaki değeri RAM'in adres kayıt edicisine yükle.
* `CE` (Chip Enable): RAM'in içindeki veriyi okuyup veri yoluna koy.
* `La` (Load Accumulator): Veri yolundaki değeri A (Accumulator) kayıt edicisine al.
* `Su` (Subtract): ALU'ya çıkarma işlemi yapmasını söyle (0 ise toplama yapar).
* `Eu` (Enable ALU): ALU'nun sonucunu veri yoluna aktar.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin, yüksek seviyedeki bir `Z = X + Y` işlemi, makine dilinde `ADD R1, R2` şeklindedir. İşlemci bu makine kodunu aldığında, mikrokod birimi devreye girerek bu komutu şu donanım sinyallerine çevirir:
1. `[Veri Yolu A'yı aç]` -> X'in değerini ALU'nun sol girişine ilet.
2. `[Veri Yolu B'yi aç]` -> Y'nin değerini ALU'nun sağ girişine ilet.
3. `[ALU Toplama Devresini tetikle]` -> ALU'ya matematiksel toplama yapması için elektrik ver.
4. `[Sonuç Yolunu Z Kaydına (Register) bağla]` -> Sonucu donanım üzerinden saklama alanına yaz.

### Örnek Bir Mikrokod Talimatı: ADD (Toplama İşlemi)
Yüksek seviyedeki bir kod makine diline, örneğin `ADD` gibi bir toplama komutuna dönüştüğünde, mikrokod birimi bu makine komutunu alıp **donanımsal saat döngülerine (Clock Cycles)** böler ve aşağıdaki gibi "sinyal setleri" çalıştırır:

```text
T0 (Fetch 1) : Ep, Lm        // Program Counter değerini (Ep) Memory Address Register'a (Lm) yükle.
T1 (Fetch 2) : Cp            // Program Counter'ı bir artır (Cp = Clock PC).
T2 (Fetch 3) : CE, Li        // RAM'den Instruction'ı oku (CE) ve Instruction Register'a (Li) yaz.
T3 (Execute) : Ei, Lm        // Instruction Register'ın Adres kısmını (Ei) Memory Address Register'a (Lm) yaz.
T4 (Execute) : CE, Lb        // RAM'den toplanacak sayıyı oku (CE) ve B Register'a (Lb) yükle.
T5 (Execute) : Eu, La        // ALU'nun toplama sonucunu (Eu) Accumulator'a (La) yaz. (Toplama işlemi bitti)
```

Görüldüğü gibi, makine dili seviyesindeki tek bir komut, gerçekte işlemci içerisinde kapıların belli bir sırayla elektrik aldığı `[Ep, Lm]`, `[Eu, La]` gibi **Mikrokod** kontrol dizileriyle fiziksel olarak icra edilir. Gerçek bir Intel veya AMD işlemcisinde bu sinyal kelimesinin (Control Word) uzunluğu yüzlerce bit boyutunda olabilir.

**Kimler Kullanır?**
* Geleneksel hiçbir yazılımcı veya işletim sistemi geliştiricisi mikrokod yazmaz. 
* Yalnızca çip tasarımcıları ve işlemci mimarları (Intel, AMD, Apple donanım mühendisleri vb.) tarafından, tamamen o işlemci mimarisine **özel** olarak yazılır ve üretimde çipin ROM (Sadece Okunabilir Bellek) kalıbına gömülür.
