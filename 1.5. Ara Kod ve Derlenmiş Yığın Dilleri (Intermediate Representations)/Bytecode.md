# Bytecode (Ara Kod)

## Özet
Bytecode; Ne insanların yazıp okuyabildiği (Python/Java gibi Yüksek Seviyeli), Ne de İşlemcinin (Intel/AMD) doğrudan anlayabildiği %100 saf makine dili (1 ve 0'lar) olan... İkisinin TAM ORTASINDA kalan, sadece özel bir **"Sanal Makine (Virtual Machine)"** tarafından okunarak eşzamanlı çalıştırılmak(Yorumlanmak) üzere **Derleyici tarafından kısmen (Yarı yarıya) ezilmiş / sıkıştırılmış Yapay (Ara) Makine Kodudur.** Java (.class) ve Python (.pyc) dillerinin Dünyaya yayılmasını sağlayan Köprüdür.

## Nedir ve Ne İşe Yarar?
Eskiden (C++ kullanırken) Kodu derlediğinizde çıkan `.exe` dosyası sadece Windows'ta (Intel işlemcide) çalışırdı. O EXE'yi alıp Mac (Apple) bilgisayara veya Akıllı Telefona atarsanız "İşlemci Mimarisi (Komut Seti) Uyumsuz!" diyip Çalışmazdı. Codeların her cihaz için BAŞTAN yazılması gerekiyordu.

1995'te Java Devrimi Oldu. Java dediki: **Sizin Makinenize (Windows/Linux) göre EXE üretmeyeceğim! Ben (Bytecode) adında Kendi Sanal Bilgisayarımın dilini üreteceğim.**
Siz Java (veya Python) kodu yazdığınızda Arka planda bir `Bytecode` Üretilir. Bu bytecode Cihaza Bağlı DEĞİLDİR! O bytecode'u Mac'e de atsanız, Buzdolabına da (Eğer Cihazın içinde JRE- Java Sanal Makinesi Yüklüyse) Sorunsuz Ve Hatasız çalışır!
Çünkü İçerdeki JRE(Sanal Makine), Bytecode'u okuyup onu gerçek anlık(JIT) İşlemci diline Tercüme Eder. Evrensel Çalışabilirlik (Write Once, Run Anywhere) budur.

**Ne İşe Yarar?**
* **Çapraz Platform (Evrensellik):** Python'da yazdığınız Kodu Arkadaşınıza attığnıızda, Python derleyicisi Onu Gizlice `__pycache__` klasörüne (Ekranda gözükmeyen .pyc koduna-Bytecode) dönüştürür. Bu Sayede Pyhton Mac'te de Windows'ta da AYNI Bytecode üzerinden hızla Akmasını (İşlenmesini) sağlar.

## Dilin Mantığı ve Kod Yapısı
Siz Bytecode Yazmazsınız. Compiler Bunu Kusar. Aslında Assembly dilinin (MOV, PUSH, POP) Aynısıdır, ANCAK fiziksel İşlemci (Hardware) Mimarisi (Yazmaçlar-Registers) Yerine; RAM üzerinde **Yığın (Stack)** Bazlı Suni(Sanal) Bir Makine için Dizilmiş Emirlerdir!

### Örnek Bir Bytecode Anatomisi (Java Sanal Makinesi - JVM için!):
Java'da siz İki Sayıyı Toplayan `int c = a + b;` Kodunu yazdığınızda, O kod `.class` (Bytecode) dsosyasının içince Kısacık (İnsan Gözüyle Çok ucube olan) Tek byte'lık Emirlere Düşer:

JVM Bytecode Dizilimi Böyle Yakar (Sayiyla / Opcode İle Temsil):
* `0x1B`: (Bunun Anlamı LLOAD_1 dir -> Stack'teki(Masadaki) 1 Numaralı Degişkeni Hafzıaya Al!).
* `0x1C`: (LLOAD_2 -> Masadaki 2 numarali Degiskeni Hafizeya Al!).
* `0x60`: (IADD -> ALınanan O İki Rakamı TOLA!).
* `0x3C`: (ISTORE_3 -> Toplanan Sonucu 3 Numaralı Çekmeceye(Deigsene) Geri Koy!!). 

Pythonda Siz Terminalee Şunu Yazarsanız Pyhtonun Beyninin(Byteocodunn) Icine Girerresiniz : `import dis; dis.dis("a = 2 + 2")` 
Makinenin Şunu(Bytecode) Yaptığını Görürsünüz:
```text
  1           0 LOAD_CONST               0 (4)  <-- Python Akillidir, Kodu onceden ezmis, DIREK (4)u hazilrais.
              2 STORE_NAME               0 (a)  <-- A degiskeninin ICINE BAS (Kutuya Sakla)!
              4 LOAD_CONST               1 (None)
              6 RETURN_VALUE 
```

Gerçek CPU (İntel) İşlemcisi bu "LOAD_CONST" yazan şeyi Çalıtıramaz. Pthyton(Cpython Sanal Makinesi - Yabancı Elçi) onu Alır.. Pıtı Pıtı İşelmcniin dilien CEvrrir.. Istr Bu 2. Katmana Ara Kod(Byteccoce) Denir!

## Kimler Kullanır?
* Kendi Programlama Dilini Yaratan (Tasarımcı) **Derleyici (Compiler) Mühendisleri**. Eğer Siz bir Dilicat ederseniz Bütün Intel ve AMD Cİhazların 70 Yıllık Komut stetleerinii Öğrneemzsiiniz. Dersnizki "Ben Kendi Bytecodumu yaratiyioyurm (LLVM İle)" O gidin HAngi islmciyie İstyorsa Kendii donsun!
* Programların Tersine Mühedisliğini Yapan Hackerlar(Java .jar veya Android .Apk kırarken o Dex Bytecodlerinii Okuyarark Gİzli Sifreyi CöZElrler. Programlamnini ArA-Araf'idir.
