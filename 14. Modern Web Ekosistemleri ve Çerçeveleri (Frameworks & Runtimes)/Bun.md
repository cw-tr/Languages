# Bun

## Özet
Bun; 2022 yılında Jarred Sumner tarafından piyasaya sürülen, Node.js ve Deno'nun karşısına **"Dünyadaki EN HIZLI JavaScript / TypeScript Runtim'i (Çalışma Zamanı)"** olma iddiasıyla dikilen ve C++ / Rust yerine Sistemin tam kalbindeki **ZIG programlama dili (ve Apple Safari'nin JavaScriptCore Motoru)** ile inşa edilmiş Uçak/Işık Hızında bir Ekosistem Değiştirici (Disruptor) Teknolojidir.

## Nedir ve Ne İşe Yarar?
10 Yıldır Backend(Sunucu) JavaScript pazarında "Motor (Engine)" olarak Sadece Google Chrome'un **V8** Motoru (Nodejs ve Deno'nun kalbi) kullanılıyordu. V8 muazzamdı ancak Başlangıç(Boot) performansı Bazen Yavaş kalabiliyordu. Ayrıca Node.js ekosistemi çok yavaş çalışan, Paket indirirken (NPM Install) insanı 3 dakika ekranda Kahve içmeye mahkum C++ bağımlılıklarıyla doluydu.

Jarred Sumner; "V8 Motorunu Çöpe Atıyorum! Yerine Apple'ın Safari'sinde kullanılan (Çok daha Hızlı Başlayan) **JavaScriptCore (JSC)** motorunu koyacağım. Bir de Sistemin Etrafındaki Dosya okuyucuları (Node Çevresini) C++ Yerine Düşük seviye ve Mükemmellikteki **ZIG (C'nin katili)** diliyle kodlayacağım!" dedi. Ortaya Çıkan BUN (Çörek Cisimkli Logonun Aıd), Node.js den **3 ila 5 KAT Daha Hızlı Çalışan** Bir Mucize oldu!

**Ne İşe Yarar?**
* **Performans Kralı (API & Backend):** Saniyede Gelen 100 Bin Adet HTTP İsteğine (Request) Aynı Donanımda Cevap verme Kapasitesine ulaştığı Kıyaslama(Benchmark) testlerinde, Nodejs'in V8'ini ikiye katlayıp Evrenin Hakimi unvanını Kazanmıştır.
* **Tam İstif İsveç Çakısı (All-in-One Toolkit):** Bun Sadece bir Runtime(Node) Değildir! O Aynı Zamanda Bir "Paket Yükleyici (NPM'in 30 Kat Hızlısı)", Bir "Test Makinesi (Jest'in klonu)", Bir "Paketleyici (Webpack Katili)". Hepsi Tek 1 ".exe/Dosya" içindedir!

## Dilin Mantığı ve Kod Yapısı
Dili DOĞRUDAN **TypeScript / JSX ve JavaScript**'tir. 
Node.js Programlarındaki Tüm Dosyaları (`package.json`, NPM ekositemi) Kökünden **Destekler (Drop-in Replacement)**. Yani bir şirketin Varsa Mevcut bir Nodejs projesi, Kodları değiştirmeden Sadece Konsola `node koum` yerine `bun kodum` Yazarak Projeyi Işık Hızına İletebilir. (Deno gibi NPM'yi reddetmez, Onu İÇİNE ALARAK Ezer Geçer).

### Örnek Bir Bun Mimarisi: Dahili HTTP Motoru İle Işık Hızında Server
Node.js'te "Express" yada "HTTP" indirmek gerekirken, Bun Kendi İçine o Kadar Çok (Optimzie/Zig Kodlu) Yerel Kütüphane Gömmüştür ki Sadece Şu kadarcık kod Bir Sunucu Açar:

```typescript
/* BU BIR BUN (TypeScript) KODUDUR */

// Herhangi Bir Kutuphane Indirmeden, Bun'in "Serve(HizmetEt)" Sihirli Objesini Çagirin!
const sunucum = Bun.serve({
    
  // 1. PORT Ayari (İnterneti Dinleme Kapisi)
  port: 3000,
  
  // 2. FETCH (Internetden Gelen Istege Cevap Uret) 
  fetch(istek_geldi) {
      
    // Gelen İstegin Hangi Sayfaya(URL) Geldigini Ogrern:
    const sayfaUrli = new URL(istek_geldi.url);
    
    // Eger Anasayfaya Girdiyse adama Metin Don:
    if (sayfaUrli.pathname === "/") {
      return new Response("🚀 Bun Isık Hizindan Opeerek Selamlar Vurur!");
    }
    
    // Eger JSON sayfasina Geldiyse adama HIZLI C++ (ZIG) JSON uretimi at!
    if (sayfaUrli.pathname === "/gizemliAyar") {
      return Response.json({
          motor: "JavaScriptCore",
          dil: "TypeScript Dogustan Supportlu",
          statu: "Mukemmel Hiz"
      });
    }

    // Yoksa 404 sayfasi
    return new Response("Dosya Yok (404)!", { status: 404 });
  },
});

console.log(`BUN Sunucusu Aktif! Port Su An: ${sunucum.port}`);
```
Bu dosya kaydedilip terminalde SADECE `bun run index.ts` Yazılarak Başlatıldığında (Arka PLanda Tsc Derlemesine Gerek Yok, Kendisi Ototmatik TS Okuru), Milisnaiyede (Saniyenin onda biri gibi bir sürede) Sunucu Ayakaltıp Dinlemeye Geçer. (Nodejs de bu 1-2 Saniye sürebilldiğinden Ciddi Performans farkı oluşturur).

## Devrimi (Neden Çok Popüler Oldu?):
Sıradan bir Yazılımcı Bilgisayarına Bir Şey Yuklerken `npm install react` yazar Ve 40 Saniye Klasörlerin/Çöplerin İndirilmesini Ekrandaki Bar'ı beklerdi.  Eğer Bunu `bun install react` Şeklinde Yazarsanız **0.5 Saniyede (Ciddiyiz milisaniyeler) İnsafsız bir Bağlantı gücüyle Bütün Paketleri İndirip Cihaza kitler.** Cünkü Arka planda Rust/Zig Cekirdekli Dosya Sıkıstırma Ve Ağ-Köprüsü(Async Socket) harbindedir!

## Kimler Kullanır?
* Evrendeki bütün **"Sunucu Optimizasyonu ve Maliyet Azaltması" Pespşindeki Şirketler (Modern Full-Stackler)**. Eğer Firmanın Backend API'si Node.js te Çok ram Tüketiyorsa Veya Yavaştan Çökmeye Başlıyorsa, Yazılımcı Hemen Bilgisayarındaki Nodejs'i Sİlip Kodu `BUN` a Atar, Sorunlar Işık Şidetiiyle Çözülür.
* **Geleceğin Hakimiyetidir.** Piyasaya Cıktıgından Günden Bu yana (Github Yilditleri olarak) TeknoloJi Tarhihnin En Hizla Büyeyen Ekostistemnlerinden bırıdır. Javascript'in V8 Motorundan (Safari Motoruna) Geçişte Evriminde En büyük Mutasyon noktasıdr.
