# Forth

## Özet
Forth, 1970'lerde icat edilen, inanılmaz derecede küçük bellek alanlarına sığabilen ve "Yığın" (Stack) mantığıyla çalışan efsanevi bir düşük seviyeli dildir. Sözdizimi o kadar farklı ve donanıma yakındır ki, çoğu zaman işletim sistemi olmadan doğrudan çıplak donanım (bare-metal) üzerinde çalıştırılabilir.

## Nedir ve Ne İşe Yarar?
Tüm diller kodlarını işletim sistemi üzerinden RAM'e gönderirken, Forth aslında kendi içinde mini bir işletim sistemi, bir derleyici ve komut satırı barındırır. Son derece ilkel ama bir o kadar da güçlü olan "Kelime" (Word) adı verilen yapı taşlarından komutlar inşa eder.

**Ne İşe Yarar?**
* **Aşırı Kısıtlı Ortamlar:** RAM ve işlemci gücünün kilobaytlar (hatta baytlar) seviyesinde ölçüldüğü uzay sondalarında, gömülü sistemlerde (embedded systems) devasa işletim sistemleri yerine doğrudan çipi yönetir.
* **Etkileşimli Donanım Kontrolü:** Yazılımı derleyip çalıştırmak için beklemek yerine, Forth'un kendi konsoluna "Ledi yak" anlamına gelen kelimeyi yazdığınız an donanım milisaniye gecikme olmaksızın tepki verir. Anında donanım testi yapmanın en eski ve saf yollarından biridir.
* **Astronomik Güvenilirlik:** Aşırı basit "Ters Polonyalı Gösterimi" (Reverse Polish Notation - RPN) sayesinde derleyicide hata çıkma lüksü sıfıra yakındır.

## Dilin Mantığı ve Kod Yapısı
Forth programcısı, bir matematiksel formül gibi satırlar yazmaz. Bunun yerine elinde devasa bir pano (Stack - Yığın) vardır. Sayıları bu panoya koyarsınız, işlemi söylediğinizde o işlemi en tepedeki sayılar üzerinde uygular ve yeni sonucu oraya bırakır. Buna *Ters Polonyalı Gösterim (RPN)* denir. C, Java gibi dillerin aksine parantez ya da işlem önceliği (çarpma toplamadan önce gelir gibi) kuralı yoktur. 

Aynı zamanda Forth "esnek" değil "kelime tabanlı" bir dildir. Mevcut kelimeleri birleştirerek kendi kelimenizi icat edersiniz ve dil o an sizin için evrimleşmiş olur.

**Örnek İşleyiş (Sembolik Olarak):**
Normal (Infix) matematikte "3 ile 4'ü topla, sonra 5 ile çarp" demek için `(3 + 4) * 5` yazarsınız. Forth'da (RPN mantığıyla) ise işlem şöyle ilerler: `3`'ü yığına at. `4`'ü yığına at. `+` komutunu ver (son iki sayıyı yığından al, toplayıp `7` olarak yığına koy). Sonra `5`'i yığına at. `*` komutunu ver (Yığındaki `7` ile `5`'i çarpıp sonucu bırak).

### Örnek Bir Forth Kodu: 
İki sayının karesini alıp toplayan (Yani bir dik üçgenin hipotenüs denklemini, X^2 + Y^2 kuran) bir Forth "kelimesi" uyduruyoruz:

```forth
\ "\" işareti ve parantezler Forth dilinde yorum satırıdır.
\ Yeni bir kelime tanımlamak için başına ":" sonuna ";" konur.
\ Yığının o anki en üstteki 2 elemanını alır.

: KARELER_TOPLAMI ( x y -- sonuc ) 
  SWAP DUP *       \ En üstteki sayıyı yedekle, kendisiyle çarp (Karesini al)
  SWAP DUP *       \ Diğer sayıyı üste çek, onu da kendisiyle çarp (Onun da karesini al)
  +                \ En son oluşan bu iki kare sayıyı birbiriyle topla
;

\ Programın çalıştırılıp test edilmesi:
\ Yığına sırasıyla 3 ve 4 sayılarını koyuyoruz ve uydurduğumuz komutu çağırıyoruz.
3 4 KARELER_TOPLAMI . 

\ Program Çıktısı: 25   (Nokta '.' komutu yığının en üstündekini ekrana basar)
```

Bu şekilde sistemdeki komut dağarcığınızı tuğla dizer gibi basit işlemlerle yavaş yavaş uzay mekiğini yönetecek karmaşık "kelimelere" doğru genişletirsiniz.

## Kimler Kullanır?
* NASA ve diğer uzay ajanslarının uçuş kontrol mühendisleri (Rosetta uzay mekiğinin üzerindeki Philae sondasının asteroide iniş bilgisayarında Forth kullanılmıştır; çünkü C dili bir yonga arızalanırsa kurtarılamazdı, oysa Forth anında canlı düzeltmeye izin verir).
* Modern bilgisayarların açılış yazılımları olan Open Firmware sistem programcıları (Erken dönem Apple Mac'ler, Sun Microsystems).
* Akıllı kartlar ve aşırı kısıtlı bellekli IoT çiplerinin tasarımcıları.
