# WebAssembly (Wasm)

## Özet
WebAssembly (kısaca Wasm), modern web tarayıcılarında JavaScript'in yanında çalışabilmek üzere tasarlanmış, taşınabilir (portable), boyut olarak çok küçük ve neredeyse makine hızında (native) çalışan bir **düşük seviyeli sanal Assembly dilidir**. İnternetin gelecekteki performans devrimidir.

## Nedir ve Ne İşe Yarar?
Tarihsel olarak web tarayıcıları sadece tek bir programlama dilini (JavaScript) çalıştırabiliyordu. Ancak JavaScript yüksek seviyeli ve yorumlanan bir dil olduğu için 3D oyunlar, ağır video işleme veya karmaşık kriptografi gibi güç gerektiren işlerde işlemciyi aşırı yorar. 

WebAssembly bu sınırlamayı yıkarak; C, C++, Rust ve Go gibi güçlü dillerde yazılan yazılımların, doğrudan web tarayıcısının içine indirilip, güvenli (sandbox) bir ortamda yerel bilgisayar donanım hızıyla (CPU'ya çok yakın bir katmanda) çalışmasını sağlayan bir makine dili standardıdır.

**Ne İşe Yarar?**
* **Ağır İş Yüklerini Web'e Taşıma:** Photoshop, AutoCAD, Unity oyun motoru gibi normalde masaüstüne yüklenmesi saatler süren ve yüksek işlemci/bellek isteyen gigabaytlarca uygulamayı, hiçbir eklenti kurmadan "sekme içinde" anında açabilmenizi sağlar.
* **JavaScript ile Omuz Omuza:** JavaScript'i öldürmek için değil, onun zayıf olduğu kas gücü gerektiren yerlerde devreye girmek için tasarlanmıştır. JavaScript tıklamaları/arayüzü yönetirken, Wasm ağır matematiği çözer.
* **Güvenli ve Taşınabilir Performans (Write Once, Run Anywhere):** Tıpkı gerçek donanım Assembly'si gibi yığın tabanlı (stack-based) hızlı bir sanal makine sunar. Üstelik işletim sistemi veya CPU mimarisi (Intel, ARM, Apple Silicon) fark etmeksizin aynı hızda çalışır.

## Dilin Mantığı ve Kod Yapısı
Geleneksel X86 Assembly'si nasıl Intel işlemcilere özel 0 ve 1'lere dönüştürülüyorsa, WebAssembly de ".wasm" uzantılı, tarayıcının Sanal Makinesine özel bir "ikili formata" (binary format) derlenir. Bu sanal makinenin kendine ait devasa bir belleği (linear memory) ve çağrı yığınları vardır.

Gerçek donanım Assembly'sinden en büyük farkı: Wasm doğrudan fiziksel RAM'e veya işletim sistemi çekirdeğine erişemez. Katı bir güvenlik çemberi (sandbox) içinde, tarayıcının ona sınırlı olarak verdiği bellek üzerinde matematiksel hesaplarını donanım hızında halleder.

**Örnek İşleyiş (Sembolik Olarak):**
Rust veya C diliyle yazdığınız çok ağır bir filtreleme (fotoğraf siyah-beyaz yapma) fonksiyonunu Wasm olarak derlersiniz. Tarayıcıya girdiğinizde, arka planda bu Wasm dosyası mili saniyeler içinde indirilir. Kullanıcı butona bastığında, JS bu fonksiyonu çağırır ve görsel tarayıcı sekmesi çökmeksizin yıldırım hızında filtrelenir.

### Örnek Bir WebAssembly Kodu (WAT Formatında): Toplama
İnsanların 0 ve 1 uzantılı `.wasm` dosyalarını okuyabilmesi imkansız olduğundan, WebAssembly'nin insanlar tarafından okunabilen/yazılabilen harfsel bir metin formatı daha vardır: **WAT (WebAssembly Text Format)**. 

Lisp diline çok benzeyen "S-Expressions" (Parantezli ifadeler) kullanır. Aşağıda dış dünyaya "add" ismiyle açılan, iki sayıyı alıp toplayan bir Wasm (WAT) modülü vardır:

```wat
(module
  ;; Gelen iki 32-bit'lik (i32) tam sayıyı (p1 ve p2) alan ve i32 döndüren "add" adında bir fonksiyon dışa aktar/export et.
  (func (export "add") (param $p1 i32) (param $p2 i32) (result i32)
    
    local.get $p1   ;; 1. parametreyi Sanal Yığına (Stack) al
    local.get $p2   ;; 2. parametreyi Sanal Yığına al
    
    i32.add         ;; Yığındaki en üstteki 2 sayıyı alıp topla ve sonucu tekrar yığına koy.
  )
)
```
Bu kod satırları derlendiğinde, tarayıcınıza gönderilecek küçücük bir `hesap.wasm` (0 ve 1'ler) dosyasına dönüşür. Tarayıcı motoru (Örn: V8 Engine) bu Wasm dosyasını gördüğü an "Bu zaten alt seviye, kontrol etmeye gerek yok" diyerek doğrudan bilgisayarınızın işlemcisine (CPU) uygun gerçek makine diline çevirip şimşek hızında çalıştırır.

## Kimler Kullanır?
* C/C++ ve özellikle Rust dillerini kullanarak Figma gibi saniyede 60 kare (60 FPS) çalışan devasa web tabanlı uygulamalar, tasarım araçları ve oyun motorları yazan mühendisler.
* Güvenli akıllı sözleşmeler (Smart Contracts) oluşturmak için Blockchain (Ethereum, Polkadot vb.) geliştiricileri.
* Konteynerler yerine (Docker vb.) çok daha hafif, hızlı uyanan ve bulut üzerinde milisaniyede çalışan sunucusuz (Serverless Edge Computing) fonksiyonları yazan bulut mimarları.
