# Makine Dili (Machine Code)

## Özet
Makine dili, işlemcinin (CPU) doğrudan ve kendiliğinden anlayıp çalıştırabileceği, sıfırlar ve birlerden oluşan mutlak ve yegane komut dilidir. Evrendeki en donanımlı yazılımlar dahi son aşamada makine diline dönüşmek zorunda olan yüksek seviyeli hayallerdir.

## Nedir ve Ne İşe Yarar?
Bildiğiniz C, Java, Python gibi üst seviye dillerde yazılan tüm devasa yazılımlar aslında işlemci için bir hiçtir; ta ki "Derleyiciler" bu kodları Makine Dili'ne çevirene kadar. Bu dil, anakart ve işlemci içindeki dijital iletim hatlarına gönderilen (5 Volt ve 0 Volt şeklindeki) mikro elektrik sinyallerini (bitleri) yazılımsal olarak analiz eder.

**Ne İşe Yarar?**
* **Sistemin Gerçek Dili (Native Code):** Bilgisayarda aklınıza gelebilecek her işlemin (fareyi hareket ettirmek, hafızadan veri çekmek, sayemizde ekranda pikseller oluşturmak) ana kart üzerindeki gerçek itiş ve çekiş gücüdür. İşlemci sadece ve sadece Makine Dilini anlayıp icra(execute) eder.
* **Saf Hız (0 Overhead):** İşlemci bir komutu alırken önce araya giren başka yazılımlarla süzmeden, "doğrudan" çalıştırdığı için hızı 1 birime çıkaran taban referanstır. Herhangi bir sanal makine veya tercümana ihtiyacı yoktur.
* **Total Hakimiyet:** Çekirdeğin RAM'deki istediği korumalı belleğe (izolasyonu kırmayı başarabildiği sürece) dokunmasını sağlayan direkt yoldur.

## Dilin Mantığı ve Kod Yapısı
Zihnimizde oluşan değişken isimleri ya da nesneler bu dilde yoktur. En temel Makine Dili talimatı temelde iki kısımdan oluşur:
1. **İşlem Kodu (Opcode - Operation Code):** Topla, çıkar, taşı veya zıpla gibi işlemcinin ne yapması gerektiğini tanımlayan ikili kimlik dizisidir. 
2. **İşlenenler (Operand):** O işlemin uygulanacağı kayıt edicileri (register) veya somut RAM adreslerini belirten değerlerdir. 

**Mimari Ayrım (ISA - Instruction Set Architecture):**
Önemli bir kural; "Makine Dili Evrensel Değildir!" Apple'ın M serisi cihazındaki çip (ARM Mimarisi) ile evdeki oyuncu bilgisayarındaki Intel çipinin (x86 Mimarisi) 0 ve 1 dili tamamen başkadır. C programını x86 işlemci makine koduna dönüştürdükten sonra o çalıştırılabilir (.exe) dosyayı ARM'e atarsanız sisteminize "bu 0 ve 1 dizisi benim Opcode listemde yok" diyerek tamamen bir çöp serpiştirmesi yapacaktır.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin, "işlemcinin geçici beyni" olan bir yazmaca 5 eklenecek. Programlama dünyasının en ilkel komutu. İnsanın yazığı koda derleyici müdahale eder ve bunu saf bir ikili satıra gönderir: `Bu satır bir işlemdir + Hangi İşlemdir (Toplama) + Nerede Yapılacaktır (Ax Yazmacında) + Değer Nedir (5)`. İşte makine dili donanıma bu kadar parçalı komut yağdırır.

### Örnek Bir Makine Dili Kodu: Toplama
Intel x86 mimarisinde işlemcinin kalbindeki `AX` isimli hafıza ünitesine `5` rakamını eklemek gibi basit bir şeyin bellek üzerindeki net, dijital karşılığı şu `ikili` (binary) dev sinyal dizisidir:

```text
00000101 00000101 00000000
```
Ancak insanlar, 0 ve 1'lerden oluşan sonsuz bir denize bakmaktan kaçınmak ve hataları bulabilmek için bilgisayar makine kodunu 16'lık tabanda, yani **Onaltılık (Hexadecimal)** denilen çok daha kompakt formda yazar ve incelerler. Aynı kodun hex karşılığına bakalım:

```text
05 05 00    
```
İşte bir makine kodu. İlk parça (`05`) işlemcinin "bu bir Ekleme/ADD komutudur ve AX'e yapılır"ı anladığı Opcode, diğerleri ise eklenecek 5 sayısının bellek kodudur. İçinde İngilizce "ADD" ya da "MOVE" yazmaz, saf `0x05` byte'ları uçar.

## Kimler Kullanır?
* Evrendeki hiçbir programcı baştan aşağı "0 ve 1'ler (yada Hex) dizerek" bir oyun stüdyosunda oyun çıkarmaz veya muhasebe yazılımı yapmaz. (Sadece 1940-50'lerde ENIAC dönemindeki anaokul dâhileri, bilgisayarlara anahtarlarla manuel voltaj vererek programlama yapıyordu.)
* **Derleyiciler:** Makine dilinin temel ustası derleyicilerdir (örneğin GCC, Clang). Yüksek seviye dilleri (C++) bu seviyeye onlar tercüme eder.
* **Tersine Mühendisler (Reverse Engineers):** Crackers, siber güvenlik analistleri ve antivirüs zarılım avcıları incelenecek şüpheli bir exe'nin hexadecimal dizilerine bakarak onun ne işler karıştırdığını anlarlar.
