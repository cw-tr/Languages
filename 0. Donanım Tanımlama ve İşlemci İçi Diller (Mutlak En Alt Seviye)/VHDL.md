# VHDL

## Özet
VHDL, donanımın fiziksel davranışını ve yapısını kod satırlarıyla ifade etmek için kullanılan üst düzey bir Donanım Tanımlama Dilidir (Hardware Description Language - HDL). Yazılım yazmak yerine, elektronik bir devre tasarlamanızı sağlar.

## Nedir ve Ne İşe Yarar?
VHDL (VHSIC Hardware Description Language), silikon üzerinde tasarlanacak olan FPGA (Field Programmable Gate Array) ve ASIC (Application-Specific Integrated Circuit) gibi tümleşik devrelerin mantıksal mimarisini tarif eder.

**Ne İşe Yarar?**
* **Dijital Devre Tasarımı:** Mantık kapısı (gate) seviyesinden başlayarak, en karmaşık işlemcilerin ve bellek birimlerinin fiziksel bağlantılarını ve davranışlarını kodlayarak simüle eder.
* **FPGA Programlama:** Boş bir elektronik çip olan (yani içinde transistörleri olan ama transistörlerin bağlantıları henüz yapılmamış esnek levha) FPGA'leri, istenilen donanımsal mantığı gerçekleştirecek şekilde "bağlamak" (configure etmek) için kullanılır.
* **Donanım Doğrulama (Verification):** Devreler gerçek silikon üzerine basılmadan (üretilmeden) önce sanal ortamda sinyallerin doğru çalışıp çalışmadığını test eder.

## Dilin Mantığı ve Kod Yapısı
VHDL, yazılım programlama dillerinden farklı olarak "sıralı" (sequential) değil, temelde "eşzamanlı" (concurrent) çalışır. Çünkü elektronik bir devrede, gerilim uygulandığında elektrik yolu üzerindeki tüm kapılar aynı anda tepki verir. (Tıpkı bir elektrik panosunda şalteri kaldırdığınızda bağlı tüm lambaların aynı anda yanması gibi).

Kodda `entity` (devrenin pinleri/dış bağlantıları) ve `architecture` (devrenin iç davranışı) olmak üzere iki temel mimari kavram vardır.

**Örnek İşleyiş (Sembolik Olarak):**
Elinizde iki girişli (A ve B) ve bir çıkışlı (Y) donanımsal bir "VE" (AND) kapısı devresi olduğunu düşünün. VHDL, önce bu donanım parçasının giriş ve çıkış bacaklarının adını ve türünü tanımlar. Ardından içindeki sinyal akışının "A ve B ise Y olsun" şeklinde davranmasını kurala bağlar.

### Örnek Bir VHDL Kodu: Basit AND Kapısı

```vhdl
-- 1. ADIM: Devrenin dış dünyayla bağlantı noktalarını (Pinlerini) "entity" olarak tanımlıyoruz.
entity AND_KAPISI is
    Port ( A : in  STD_LOGIC;  -- "A" elektriği alan donanımsal bir giriş (input) pini
           B : in  STD_LOGIC;  -- "B" elektriği alan donanımsal bir giriş pini
           Y : out STD_LOGIC); -- "Y" devreden çıkan sonucu ileten pin (output)
end AND_KAPISI;

-- 2. ADIM: Bu pinlerin içeride nasıl birleşeceğini (Davranışını) tanımlıyoruz.
architecture Davranis of AND_KAPISI is
begin
    -- A ve B pinlerindeki voltaj durumuna göre VE (AND) mantığını uygular, sonucu Y pinine donanımsal olarak bağlar.
    Y <= A and B; 
end Davranis;
```

Bu kod normal bir derleyici (compiler) ile derlendiğinde çalıştırılabilir bir yazılım (exe) oluşturmaz. Bunun yerine "Sentezleyici" (Synthesizer) adı verilen araçlardan geçer. Sentezleyici bu kodu okur ve hedef çipin içerisindeki fiziksel kabloları/transistörleri bu AND matematiksel ilişkisini kuracak şekilde birbirine bağlayan donanım haritasını çıkartır.

## Kimler Kullanır?
* Elektronik ve Donanım Mühendisleri.
* Havacılık, savunma sanayii, radar sistemleri tasarımcıları, telekomünikasyon cihazı üreticileri ve elektronik araç beyni (ECU) programcıları.
* Modern işlemci (CPU/GPU) tasarımcıları ile ağ anahtarlama sistemleri (switch/router) üreten firmalar (Intel, AMD, Cisco).
