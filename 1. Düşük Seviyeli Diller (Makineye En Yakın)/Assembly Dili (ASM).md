# Assembly Dili (ASM)

## Özet
Assembly dili; makine diline doğrudan bağlı olan, 0 ve 1'lerin dehşetengiz okunamayan yapısını basit ve kısa hatırlatıcı komutlarla (Mnemonic kelimelerle) yazılabilir hale getiren ilk sembolik "insan" programlama dilidir. Yazılım dünyasının ana kaya tabakasıdır.

## Nedir ve Ne İşe Yarar?
İşlemciye "git 01010100 sinyalini ateşle" demek yerine, insanların anlayabileceği kısaltmalar (ADD, MOV, JMP gibi) kullanılarak donanıma direkt talimat gönderilmesini sağlar. Yazılan Assembly kodu, "Assembler" adı verilen çok yalın bir süreç uygulayıcısı program ile işlemcinin o saniye anlayacağı makine diline birebir oranında (1'e 1 çevrilerek) aktarılır.

**Ne İşe Yarar?**
* **Çekirdek - Kernel Geliştirme:** İşletim sistemlerinin bilgisayar donanımıyla ilk elektrik alışverişine girdiği o kilit "Bootloader (Önyükleme)" anı, işlemcinin en kuytu hata bayrakları (Interrupt) sadece Assembly'de yazılabilir.
* **Ultra Performans:** Özellikle oyun motorlarının temel grafik işlemlerinde, büyük veri/sinyal analizlerinde ve ağır şifreleme kırıcı algoritmalarında araya Assembly optimizasyonları katılır; C veya C++ dilinin hızı dahi yetersiz kaldığında milisaniyeyi kurtaran altın kurşundur.
* **Exploit ve Tersine Mühendislik:** Kodları saklanmış ve derlenerek 0 ve 1 yapılmış (Exe/Binary) dosyalar deşifre edildiği zaman (Disassemble/Decompile), ilk karşılaşılan anlaşılır dil Assembly halidir. Virüs analistleri kodları bu haliyle analiz edip ne çaldığını ifşa eder.

## Dilin Mantığı ve Kod Yapısı
Bu dili yazmakta C, Python veya Java'daki gibi "Büyük bir nesne oluşturayım, 100 veriyi saklasın, fonksiyon hepsini halletsin" gibi devasa lüks mimariler yoktur. Bellek RAM veya bellek adresleri elle yönetilir ancak asıl sanat işlemcilerde bulunan **Register (Yazmaç/Cep)** mantığında yatar. 

İşlemcinin içine dizili ufak süper-hızlı "cepler" vardır (Ax, Bx, R15 gibi isimlerde). Toplama veya ekleme mi yapacaksınız? Önce 5 sayısını R1 cebinize taşırsınız, sonra "R1'deki bu iki cebi topla" emrini verdirir, ardından çıkacak sonucu yine donanıma siz kendiniz eliyle RAM masasına ittirirseniz çalışır.

Makine dilinin insansılaştırılmış versiyonu olduğu için aynı katı kadere mahkumdur: **Evrensel Değildir**. Her bir işlemcinin (ARM, x86, MIPS) kendine ait tamamen farklı bir Assembly sözlüğü ve grameri vardır.

**Örnek İşleyiş (Sembolik Olarak):**
Ekrana "Merhaba" yazdırmak sanılanın ve yüksek seviyeli dillerin aksine tek satır değil devasa bir iştir. Assembly'de en klasik iş değişken/hafıza transferidir:
1. Rakamı RAM'in rastgele adresinden CPU'nun içine, örneğin EAX yazmacına "taşı" (Move - MOV).
2. Matematiksel komutu uygula.
3. Çıkan o sonucu EAX yazmacından RAM'in gösterdiğin bellek alanına taşıyarak kitleştir.

### Örnek Bir Assembly Kodu: x86 Mimarisinde Değer Toplama
Mesela x86 ailesi işlemcilerde çok basit iş yapan, hafızada değerleri oluşturup toplatan bir parçayı yazılım mühendisi şöyle yazar:

```assembly
; (Noktalı Virgüller satır açıklamalarıdır ve bilgisayarca derlenmez)
section .text
    global _start       ; İşletim sisteminin programa başlayacağı etiket noktası

_start:
    ; Register (Yazmaçlara) Değerleri koyalım
    MOV EAX, 15         ; İşlemci içindeki süper hızlı EAX yazmacına 15 tam sayısını yükle.
    MOV EBX, 25         ; Hemen yanındaki EBX yazmacına da 25 tam sayısını yükle.

    ; Şimdi asıl İşlemi yapalım
    ADD EAX, EBX        ; EAX ile EBX içinde ne varsa matematiksel topla, neticeyi ilk yazlan olan EAX'e kaydet.
                        ; Artık matematik dünyasında EAX yazmacının içi donanımsal olarak 40 değeri oldu.

    ; Programı düzgün şekilde sonlandırmak ve kitlememek için işletim sistemine haber ver: 
    MOV EAX, 1          ; Sistem çağırısı (Syscall) kodu: 1 (Exit / Çıkış anlamına gelir Linux x86'da)
    MOV EBX, 0          ; Program hatasız bitti durum kodu: 0 (Return Zero)
    INT 0x80            ; Linux işletim sistemi çekirdeğine "kesme/interrupt" atıp 'bitir işimi' diye uyar.
```

Kısacık kod parçası aslında donanımın içinde nasıl satranç taşı oynatır gibi yer değiştirmelerin titizlikle emredildiğinin bir yansımasıdır.

## Kimler Kullanır?
* İşletim sistemi (Windows, Linux, Unix) kod tabanını yazan ana geliştirici mühendisler.
* Siber Güvenlik Uzmanları, Zararlı Yazılım (Malware) inceleyen Tersine Mühendisler.
* Eski konsolların (Sega, SNES, PS1) oyun motorlarını hackleyen ev yapımı (Homebrew) yama stüdyoları ve emülatör yazarları.
* Aşırı dar alanlı çiplerde programı baytlara "ezerek sıkıştırmaya" çalışan gömülü (Embedded) devre yapımcıları. 
