# Next.js

## Özet
Next.js; 2016 yılında Vercel (Guillermo Rauch) tarafından geliştirilen, Google'ın arama motoru sorunlarını (SEO engellerini) çözen, **React.js kütüphanesinin üzerine giydirilmiş Devasa Bir Meta-Framework (Çerçevelerin Çerçevesi)** mimarisidir. Ön-Yüz (Front-end) ile Arka-Yüz (Back-End) Node.js sunucusunu Tek Bir Klasörde/Projede harmanlayıp "İnternetteki modern Dev Sitelerin" en performanslı (Server-Side Rendering SSR) şekilde doğmasını sağlayan Evrensel Üst-Düzey Web standardıdır.

## Nedir ve Ne İşe Yarar?
React harikaydı, sayfayı pürüzsüz F5 atmadan çalıştırıyordu. Ama bir KABUS vardı: **SEO (Arama Motoru Optimizasyonu)**. 
Bir normal React sitesine girdiğinizde Tarayıcı "BOMBOŞ Bir Beyaz" Html sayfası indiriyordu. Sonra Javascriptler (5 Megabyte JS kodu) telefona yükleniyor, Kod çalışıp o "Boş beyaz Kağıdın İçine" Header/Menü ve Haber Yazılarını (Virtual DOM) çiziyordu (Buna **CSR - Client-Side Rendering** Müşterinin Telefonunda Renderlama dendi).

Sorun ŞUYDU: Google'ın Robot Kankası sitenize gelip "Acaba burada Hangi Haber Makalesi var?" diye okumaya geldiğinde BOŞ BEYAZ BİR EKRAN gördü. JS'lerin Yüklenmesini BEKLEMEDEN sitenizi Endeksleyip (Sıfır Çekip) Google aramalarında Dibine gönderdi!

Yaratıcılar dediler ki: "Biz bu React kodlarını (Komponentlerini) Adımın Telefonunda (Clientta) DEĞİL; **Kendi Vercel Sunucumuzda (Server-Side Backendde)** Çalıştıralım (Renderlıyalım), HTML'yi DOLU ve RESİMLERİ HAZIR Şekilde Müşteriye Fırlatalım!". Next.js ve **SSR (Server-Side Rendering)** Sihri böyle ortaya Çıktı!

**Ne İşe Yarar?**
* **SEO Dostu Full - E-Ticaret / Haber Siteleri:** Trendyol, TikTok, Twitch, Notion. Bunların arama motorlarında En üstte (Başlık ve Açıklamarıyla Metada) Çıkabilmesini Sağlayan O Dolu (Arkasında Nodejs koşturan) React'in Abisidir.
* **Full-Stack Proje (React ve Backend Bir Arada):** Siz Nextjs ile sadece Buton (UI/React) çizmekle kalmazsınız. Next'in "app/api/users" klasörlerine attığınız JavaScriptler **DOĞRUDAN** Veritabanına (SQL/MongoDB) bağlanan Arka Yüz (Backend Node.js Endpointleri) olur! Yani Front-End ile Backend Tek bir repoda Birbirine Aşık olur!

## Dilin Mantığı ve Kod Yapısı
Dili DOĞRUDAN **React (Javascript/TypeScript ve JSX)** dır. Ancak Next.js mimarisinde kod yazılmaz, "Next.js Kurallara Uyulur". (Routing ve Rendering Katmanlarıdır).

En Büyük Devrimi 1: **File System Routing (Dosya Tabanlı Rota):**
Eski React'ta "Hakkımızda" Sayfası (Kullanıcı /about Linkine tıklayınca menü gelsin demek) İçin Devasa "React-Router" xml amelelikleri dizilirdi. Nextjs dedi ki: "Masaüstündeki Klasore `app/about/page.tsx` İsminde Dosya Açıyorsan; İNTERNETTE de `/about` URL'si Otomatik olarak Yaratılıp o sayfa renderlanır. Yazılımcının Dosya Klasör Sistemi İnternet Adresi (URL Route)'udur."

En Büyük Devrimi 2: **Server Components (RSC - Sunucu Komponentleri - [App Router] Mimarisi)**
2023 Sonrası getirdiği Radikal Karar: Her şey eskiden Telefon Tarayıcısında çizilirdi (Client). Artık Sayfalar SADECE Sunucuda Veritabanından veriyi Emen Yüce kodlar (Server Component) olarak yaşar. Oraya interaktif Tuş konmaz! Eğer bir Tuş feda edeceksen başına **`"use client";`** (Bu komponenti müşterisin telefonu çizecek, Server Çizmeyecek) diye yazarsın.

### Örnek Bir Next.js Mimarisi (Server-Side Rendering & Client Side Interractivity Birlikteliği)
Kullanıcının tarayıcısında (Chrome'da) Değil; Server'da (Bulutta) tetiklenip "Hızla Database verisini indiren(Fetch) Sonra Adama Dolu HTML Şablonunu Fırlatan" modern RSC (React Server Component) Mucizesi `app/urunler/page.tsx` :

```tsx
/* BUNA DİKKAT: YUKARIDA "use client" YAZMİYORSA BU NEXTJS'DE BIR "SERVER KOMPONENTIDIR!"
   Yani Tarayıcı (Customer) Bu JavaScript kodunu ASLA görmez (Guvenlidir, Api Şifrelerin çalınmaz).
   Sunucu bunu calistirir ve Musteriye Doly Cıktı Yollar.
*/

// Modern Async Bilesen (Eskiden React Fonksiyonlarina async atilamazdi! SSR Mimarisi gucu)
export default async function UrunlerSayfasi() {
    
    // SERVER'IN İÇİNDEYİZ(Bulut): Doğrudan Amazon/Trendyol Veritababından REST API Cekisi! (1 Milisaniye Sürer, Musteri Beklemez).
    const apiIstegi = await fetch('https://bizim-gizli-db.com/api/satislar', {
         // Next.Js Cache Gucu: Bu veriyi Sunucu Kendi Hafizasina Cachesin(Ortsun) her gelene saniyede ayni sayfayi firlatsin (ISR/SSG gucu):
         cache: 'force-cache' 
    });
    
    // JSON'a cikar (Gelen Data'da yuzbinlerce Canta/Ayakkabi urunu var)
    const datalar = await apiIstegi.json();


    // JSX (HTML + REACT) RETURN'U BASLIYOR: (Bu HTML Dolu Halde Chrome'a Vurur - SEO MUKEMMELDIR)
    return (
        <main className="min-h-screen p-10 bg-gray-100">
            
            {/* SEO İçin Google'ın Bayıldığı Taglar */}
            <h1 className="text-4xl font-bold">Harika Sezon Ürünlerimiz</h1>
            
            <div className="flex flex-wrap gap-4 mt-8">
                
                {/* Database'den Gelen Urunleri Dön (Map) ve Ekrana Karta(Lego) Bas! */}
                {datalar.urunListesi.map((urun) => (
                    
                    <div key={urun.id} className="p-4 bg-white shadow-md rounded">
                        <img src={urun.resimUrl} alt="Urun Resmi" />
                        <h2>{urun.isim}</h2>
                        <p>Fiyat: {urun.fiyat} ₺</p>
                        
                        {/* 
                          DIKKAT: Buraya SATIN AL butonu koyarsam?
                          Onu ayri bir "Bileşenden (Client Component)" ithal edersiniz!
                          Cunku Server'in içinde (Kullanici Mouse'u/Alert Komutu) YOKTUR (Server tarafi kordur)! 
                          Orneklendirmek acisindan Yorumda birakilmistir. <SatinAlButonu id={urun.id} />
                        */}
                    </div>
                ))}

            </div>
            
        </main>
    );
}
```
Bu kod; Vercel (AWS) Sunucusunda çalışıp içini (HTML'nin içini Urun1..Urun2 resimleriyle) TIKA BASA GÖMÜP, Telefona (İnterneti Çok Kötü Bir Afrikalı Kullanıcıya) Fırlatır. Afrikalı Kullanıcı 5 Megabyte Javascript beklemez (Client React Sorunu); Karşısına doğrudan 20 KB'lik Full Dolu Şık HTML(CSS) sayfa 0.3 saniyede Canlı olarak yüklenir! 

## Kimler Kullanır?
* Açık konuşmak gerekirse, **Silikon Vadisinin Pırlantası, Gözbebeği ve Günümüz Sektörünün Tek Sahibi Vercel Firması (Next.js Ekosistemi)'dir.**
* Eğer Türkiye'de veya Global'de "Senior (Usta) Frontend React Geliştiricisi" iseniz; Aslında sizden %90 oranında Saf React değil, "Next.js ile Server-Side Rendering Bilen, App router kurgusundan anlayan ve Vercel Deploylarına hakim" Mühendis istenmektedir.
* Yapay Zeka Uygulamaları (Örn: ChatGPT Klonları - *Chatbot UI* vb) inanılmaz derecede sadece ve Devamlı Olarak Hız ve Full-Stack pratikliği nedeniyle Next.js ile doğar, Python FastApi arkaları ile beslenir).
