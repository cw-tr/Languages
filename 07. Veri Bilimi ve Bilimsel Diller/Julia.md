# Julia

## Özet
Julia; 2012'de MIT (Massachusetts Institute of Technology) bilim insanları tarafından, Python'un yavaşlığını, R'ın hantallığını ve C++'ın yazım zorluğunu aynı anda çöpe atarak; **"Python kadar kolay yazılsın, ama C kadar donanım hızında çalışsın"** manifestosuyla yaratılan yüksek başarılı (High-Performance) modern Veri Bilimi ve Hesaplamalı Bilim dilidir.

## Nedir ve Ne İşe Yarar?
Veri Bilimciler için yıllarca çözülemeyen "Two-Language Problem" (İki Dil Sorunu) denilen bir bela vardı. Bir bilim insanı, uçak kanadının rüzgâr simülasyonunu önce "Python" veya "Maltab" ile yavaşça yazardı (Çünkü hızlı kodlanabiliyordu). Sonra proje devasa boyuta ulaştığında bu yavaş diller yetmez, şirketin C++ mühendisleri gelip aylar boyunca o bilimsel formülleri hızlı makine makinesi diline (C++) tamamen baştan çevirirdi. Araştırmacı C++, yazılımcı ise Formül bilmiyordu, tam bir kabustu.

MIT profesörleri Julia'yı geliştirdi. **İki Dil Sorununu bitirdi**: Siz aynen Python gibi süslü parantez bile olmadan dümdüz matematik formülünü yazarsınız. Julia, C kodu gibi "AHEAD / JUST IN TIME" (LLVM motoru) ile o tertemiz kodu saniyede saf makine gücüne (0 ve 1'lere) çeker. Tıpkı C veya Fortran hızı elde edilir.

**Ne İşe Yarar?**
* **Ağır Matematik ve Simülasyon (Scientific Computing):** Meteoroloji hava tahminleri analizi, astronomların kara delik veya evren simülasyonları ve Kuantum fiziği hesaplamalarında kullanılan devasa lineer cebir matrislerine Python 50 saatte cevap verecekken Julia saniyeler içinde verir.
* **Makine Öğrenimi (Machine Learning):** Flux.jl gibi sadece saf Julia koduyla yazılmış uçtan uca Derin Öğrenme algoritmaları; C veya C++ motorları çağırmadan (Örn: Python'daki TensorFlow mecburen arka planda C++ kullanır) donanım gücünde Yapay Zeka zekâsı eğitir.

## Dilin Mantığı ve Kod Yapısı
Söz dizimi o kadar matematikseldir ki; eğer elinizde bir matematik kitabındaki $\Sigma$ (Sigma) formülü varsa, bunu klavyenizden LaTeX sembolleriyle koda **gerçek matematik sembolleri (Unicode)** olarak ekleyebilirsiniz! Dil bunu anlar (`α` harfini değişken yapabilirsiniz veya `3x`'i `3*x` anlamında çarpım kodlayabilirsiniz).

En devrimsel özelliği **Çoklu Dağıtım (Multiple Dispatch)** zekasıdır. C veya Java'daki Object Oriented (Sınıf odaklı) kısıtlamaları bitirir. Julia'da bir fonksiyon ismi yazarsınız (Örn: `carp()`), içine Matris, Sayı veya Karmaşık Sayı atsanız bile program o saniye en verimli donanım devresini/tipini seçer ve en alt düzey (Low-level) C gücünü kullanarak saniyede işletir. Bu dinamik tipliliğe rağmen C hızı sağlar.

**Örnek İşleyiş (Sembolik Olarak):**
Python'da 1 milyar sayıyı `for` ile toplarsanız bilgisayar sürünür, Vectorization (Örn: Numpy kütüphanesi) kurmanız gerekir. Yani tek parça hesaplamaktır. Julia ise **yerel olarak hızlıdır** (Native Fast). Vektör kütüphanesine bile muhtaç değildir, o yavaş dediğimiz düz `for` döngüsü bile C dilinin "for" döngüsü hızında donanımda fırlar.

### Örnek Bir Julia Kodu: Matematiksel Zarafet ve Çarpım Hızı
Üniversite tahtasından çıkmış gibi duran ama aslında C/Fortran performansında Derlenen (Compile edilen) bir simülasyon mantığı ve Unicode destegi:

```julia
# Yorum satırları klasik Python mantığı '#' ile işler.
# Fonksiyon çok saf matematiktir (function/end blogu ile sinirlanir).

# Gercek bir Yunan harfi sembolü (Pi, Theta veya Alfa) ile Degisken atayabilirsiniz!
# Klavyeden ters bölü ekleyip \alpha sonra TAB'a basarak bu sembol cikar:
α_carpan = 3.14  

# Klasik f(x) fonksiyonunun dümdüz koda uyarlanmis hali (Multiple Dispatch ile çalışır, 
# tip vermemize rağmen o icine gelenin Tam Sayi ya da Ondalik oldugunu anlayıp ona gore donanım secer)
function roket_hizi_hesapla(x, y)
    sonuc = (x^2 + y^2) * α_carpan
    # 'return' yazmanıza bile gerek yok, en son degisken otomatik doner
    sonuc 
end

# Devasa (Array / Matrix) Olusturmak Python vari ama Hizi C'dir!
# Bu 1.000 (Bin) elemanlı rastgele bir vektördür:
rastgele_degerler = rand(1000)

# "Nokta(Dot) Operatörü (.)" (Julia'nın sihri Vectorization).
# Normal roket hesaplama formülüne bir listenin başından sonunu sadece " . " koyarak gömeriz!
# Arka Planda donanımdaki binlerce "SIMD" çekirdeğine paralel saldirir:

simule_edilmis_hizlar = roket_hizi_hesapla.(rastgele_degerler, 10.0)

# Julia'nin ozel ekrani cikti komutlari
println("Roket Hesaplamasi Bitti. Maksimum Hiz: ", maximum(simule_edilmis_hizlar))
```

Bu özellik, dünyadaki hava-istihbarat kurumlarının (NASA dahil) C++ dilindeki ağır matematiği Julia'ya geçirmelerindeki en büyük zevk faktörüdür.

## Kimler Kullanır?
* Federal Havacılık Dairesi (FAA), uçak havada çarpışma önleme sistemi (TCAS)'i baştan aşağı Julia dilinde simüle edip kodlamıştır.
* NASA'dan astrofizik araştırmacıları ve MIT bünyesinde İklim Değişikliği modeli (CiMA projesi) simüle eden bilim adamları.
* Pfizer, AstraZeneca (Özellikle pandemi döneminde ilaç modellemeleri için) gibi hızın ve kolay okunurluğun beraber arandığı dünya devi dev araştırma şirketleri.
