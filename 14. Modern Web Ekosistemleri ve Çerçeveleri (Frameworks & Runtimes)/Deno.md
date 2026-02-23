# Deno

## Özet
Deno (De-No / Node hecelerinin Ters çevrilmiş Hali); 2018 yılında (Node.js'in de Yaratıcısı Olan) Ryan Dahl tarafından, Node.js'de Geçmişte yaptığı *Mimari Hataları Düzeltmek* Vizyonuyla sıfırdan ve Çekirdeği (C++ yerine) **Rust / V8 Engine** kullanılarak inşa edilen; Doğuştan %100 **TypeScript** destekleyen, NPM(Modül/Dosya) Gezegenini ve `package.json` Kabusunu kökünden yok eden Işık hızında İzolasyonlu(Secure) bir Arka-Plan **JavaScript / TypeScript Runtime (Çalıştırma)** Ekosistemidir.

## Nedir ve Ne İşe Yarar?
Ryan Dahl, Node.js'i piyasaya sürdüğünde 10 yıl boyunca Milyarlarca Kullanıcıya Ulaştı. Ama Bir gün Çıktı ve "Node.js Hakkında Pişman Olduğum 10 Şey" diye bir Kavramlar Konuşması (Tövbe Seansı) Yaptı. Pişmanlıkları Şunlardı:
1. Node.js'te Güvenlik Yoktu. Bir adamın yazdığı NPM paketini kurarsanız, O paket SİZDEN İZİN ALMADAN Bilgisayarınızdaki (C:\Belgelerim) Klasörünü Okuyup İnternete Sızdırabilirdi.
2. `node_modules` klasörü o kadar Eziyet (Devasa Ağır) idi ki, Bilgisayarda Karadelik oluşturuyordu. 1 Paket 1000 Pakete dayanıyor her şey `package.json` ile kitleniyordu.
3. TypeScript yazmak için "Araya Transpiler(Tsc) koyup, JS'ye derleyip, Sonra Çalıştırmak" Eziyeti vardı.

**ÇÖZÜM: DENO!** Dahl dedi ki: Yeniden Yazıyoruz!
**Ne İşe Yarar?**
* **Arka Plan Sunucu Mimarisi (Backend API):** Aynen Node.js ve Python gibi, Bilgisayarın İçinde çalışan, internet portu dinleyen Veritabanı çeken bir motordur, Lakin güvenlik Mührü ile kaplıdır.
* Mükemmel Geliştirici Deneyimi. Başka hiçbir yükleme Gerekmeden Kendinden Tümleşik Linter(Hata ayıklayıcı), Formatlayıcı, Test Motoruyla Gelir.

## Dilin Mantığı ve Kod Yapısı
Deno bir "Dil" Değildir. O Doğrudan **TypeScript(TS) ve JavaScript(JS)** okuyan bir Makinedir. 
Lakin Prensipleri (İnterneti Algılayışı) Tarayıcı gibidir.

**Devrimi 1: Güvenlik İzolasyonu (Sandbox Model)**
Deno kodunuzu Terminalde `deno run app.ts` Diye Çalıştırırsanız **Eğer Kodun İçinde MAkine Ağ Adresine Baglanmak Veya C Diskini Okumak Varsa** Sistem Deno Motoru Taradından Çöktürülür(Izin Verilmez). Çalışması için SİZİN ONA KONSOLDAN Acıkça `deno run --allow-net --allow-read app.ts` Diye Özel Kapı (Yetki) Açmanız GEREKIR! (iOS Tarzı İzin Mekanizması).

**Devrimi 2: NPM (node_modules) Çöplüğünün Ölümü! (URL Tabanlı Import)**
Node.js De Başkasının Kodunu Kullanacaksanız Önce `npm install express` İndirirdiniz. Klasörler Şişerdi.
Deno Der ki: İnternet Tarayıcısı (Chrome) Nasıl İnternetten Scripti Url İle anında cekiyorsa, BEN DE HTTP ile İndiririm! Herhangi Modül Yok!

### Örnek Bir Deno (TypeScript) Kodu: Kurulumsuz ve Dosyasız Sunucu Açmak 
Hiçbir NPM indirmesi yada Paket Yapılandırması olmadan, Direkt Terminalden Doğrudan İçe-Aktarım (URL Import) Modeliyle API (Sunucu) Açmak:

```typescript
/* BU BIR DENO TYPESCRIPT (app.ts) DOSYASIDIR */

// 1. DIKKKAAT!! PAKET YONETICISI (NPM) YOK!
// Baska birinin yazdigi Kutuphaneyi (Oak = Express Klonu)
// DOGRUDAN İNTERNET URL'SINDEN (Github/DenoLand gibi) Koda Bagliyoruz!!
import { Application, Router } from "https://deno.land/x/oak@v10.0.0/mod.ts";


// 2. SUNUCU(APP) VE ROTA(ROUTER) MANTIGI
const app = new Application();
const router = new Router();

// /merhaba Adresine (API'sinne) Giren İnsaa Yazilacak JSon Cevabi:
router.get("/merhaba", (context) => {
  // Typescript Zırhı ile donatilmis, Response Objesi
  context.response.body = {
      mesaj: "Deno Ekosisteminden Dunyaya Selamlar!",
      güvenlik: "Maksimum",
      hız: "Rust Motoru"
  };
});

// Ayarlanan Rotalari Sunucuya(Middleware) Gecir:
app.use(router.routes());
app.use(router.allowedMethods());

// 3. SUNUCUYU AYAĞA KALDIR (AWAIT Mucizesi!)
const PORT = 8000;
console.log(`🦕 Deno Sunucu Dinliyor: http://localhost:${PORT}`);

// NOT: Deno'da "En Ust Seviyede (Top-Level Await)" desteklenir. Fonskiyon icine girmeden Await calisir!
await app.listen({ port: PORT });
```

**Bu Kodu Çalıştırmak:** Sadece Terminale girip `deno run --allow-net app.ts` yazarsanız, Deno Kendi Kendine o İlk Satırdaki HTTPS Url'ine Gidip Kütüphaneyi kendi Cache(Önbellek) hafızasına çeker Ve Uygulumayı Fırlatır. "Node Modules Klasörü Uçsuz Bucaksız Kara Deliği" Evrenden Silinmiştir. 

## Kimler Kullanır?
* Modern Web'in Güvenliğine(Sandbox) Ve Typescript'in "Kurulum Eziyetinden (TsConfig vb)" kaçıp Tek Komutla her şeyi çalıştırmak isteyen **Yeni Nesil Backend Developerlar**.
* Geleceğin Edge Functions (En uç (Serverless/Vercel) Noktalarda Hızlı Uyanan Sunucular) Mimarilerinde, Çok hızlı Ateşlendiği ve Hafifleştirildiği için Dev Firmaların Radarına (Supabase Vb.) Güçlü bir şekilde girmiştir. (Günümüzde Node.js De boş durmayıp Deno'dan Olayları kopyalayarak Hızlandığını da belirtmek gerekir!).
