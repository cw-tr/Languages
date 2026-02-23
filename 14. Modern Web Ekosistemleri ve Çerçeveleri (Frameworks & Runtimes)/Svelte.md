# Svelte

## Özet
Svelte; 2016 yılında Rich Harris tarafından (eski The New York Times Grafik Editörü) icat edilen, React ve Vue gibi Tarayıcıya (Browser'a) inip de çalışan Geleneksel "Virtual DOM (Sanal Ağaç)" kütüphanelerinin tam aksine; bir **"Derleyici (Compiler)"** olan ve kodunuzu siz daha yayınlamadan(Build) önce Işık hızında Saf/Vanilla JavaScript heykellerine dönüştürüp Virtual-DOM motorunu çöpe atan, son yılların en sevilen Front-End (Ön-yüz) Reaktif Web Framework'üdür.

## Nedir ve Ne İşe Yarar?
React ve Vue.js web siteleri şahane şeylerdi. Ama bir sorun vardı: Siz o siteleri yaparken Müşterinin (Ziyaretçinin) telefonuna "Kendi Yazdığınız 100 KB" lık Kodu İndirtiyordunuz, Üstelik bir de Sizin yazdığınız kodu Anlaması için "React Engine (React.js Kütüphanesinin kendisini - 400KB)" indirmek zorundaydınız!
Ayrıca React, Ekranda Değişen tuşu bulmak İçin Sürekli "Eski Sanal Html ile Yeni Sanal Html'i" Bilgisayar RAM'inde Kıyaslayıp duruyordu (Virtual DOM Diffing). Bu, düşük model telefonlarda Web sitesini kasıyordu.

Rich Harris dedi ki: "Neden Kütüphane (Library) gönderiyoruz ki? Adamın tarayıcısına Motor(DOM Diffing) göndermek AMELELİKTİR. Ben bir Derleyici (Compiler) yazayım. Yazılımcı Svelte kodunu yazsın. Serverdan (Buildden) Çıkarken benim Derleyicim bunu Sadece **Dümdüz, Cihazı kasmayan Saf JavaScript (DOM Manipülasyonu)** dosyasına Çevirsin!". 

**Ne İşe Yarar?**
* **Ultra Hafif (Lightweight) Uygulamalar:** Svelte ile yazılmış bir site, Müşterinin telefonuna sıfır Kütüphane yüküyle(0 KB Engine) iner. Sadece Gerekli olan (Compile edilmiş) 10-15 KB'lık Vanilla JS inerek pürüzsüz Başlar.
* **React Kod Karmasasını Bitirmek:** JSX veya `useState()` kancaları yoktur. Dümdüz `let isim = "Ali"` dersiniz ve Ekrana `<h1>{isim}</h1>` yazarsınız, Bitti! Reaktifliği (Değişken değiştikçe ekranın Şişmesini) kendisi Sihirli olarak(Compiler seviyesinde) çözer.

## Dilin Mantığı ve Kod Yapısı
Tıpkı Vue.js gibi Svelte de **`.svelte`** uzantılı Tek-Dosya Bileşeni (SFC - Single File Component) yaklaşımını Kullanır. 
Üstte `<script>`, Ortada HTML, Altta `<style>`.

Ancak Svelte'in En İnanılmaz Sihri "Reactivity (Tepkisellik)" dir. React'ta Eğer Bir değişkene Bağımlı Birlkaça Matmetatiksel islem yapcasanız Çok zorlu Olaylar `useEffect` ler dönerr. Svelte'te Eğer Değişkenin başına `$: ` (Gözetleme İkonu - Dollar Label) koyarsanız, o değişken Sayfadaki herkesle anında Etkileşime geçer.

### Örnek Bir Svelte Kodu: Sayacı Arttırmak ve Reaktif Çarpım
Dünyanın (Angular veya React'a Göre) En basit, Saf (Vanilla) kılıklı Geliştirici-Dostu Front-End Tıklama Komponenti:

```html
<!-- METIN BELGESININ ADI: Sayac.svelte -->

<!-- 1. JAVASCRIPT / STATE MANTIGI (Hicbir Kütüphane IMPORT Etmeye Gerek Yok!) -->
<script>
    // Dümdüz Bir Degisken Tanimla (React'taki const [sayi, setSayi] = useState() AMeleliği Yok!)
    let sayi = 0;

    // Reactivity (Tepkisellik) Mührü: $: 
    // Bu Cizgili Dolar isareti Derleyiciye Sunu der: "Eger Sayi degisirse, İkiKati'ni ANINDA Otomatik Guncelle!"
    $: ikiKati = sayi * 2;

    // Tiklama Fonksiyonu (Yine dümdüz JavaScript fonskiyonu)
    function arttirOglum() {
        sayi += 1;
    }
</script>


<!-- 2. CSS/TASARIM BOLUMU (SCOPED: Sadece Bu Tuşu Boyar Baska Yeri Bozmaz) -->
<style>
    button {
        background-color: #ff3e00;  /* Svelte'in Marka Rengi Turuncu! */
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
    }
</style>


<!-- 3. HTML (MUSTERIYE OLAN CIKTI GORUNTUSU) -->
<main>
    <h1>Svelte Sanal DOM(Virtual DOM) Kullanmaz!</h1>
    
    <!-- Degiskenleri (Let) Html İcine Gommek İcin Sadece Surclu { } Yeterlidir -->
    <p>Su anki Degeriniz: {sayi}</p>
    <p>O Sayinin Otomatik Iki Kati: {ikiKati}</p>
    
    <!-- Tiklanilma Olayini 'on:click' seklinde JS funcisyonua Bagla! -->
    <button on:click={arttirOglum}>
        Arttirmak Icin Tikla
    </button>
</main>
```

**Arkaplanda Ne Oluyor?**
Siz bu kodu Yayına(Production/Build) gönderirken Svelte Derleyicisi Devreye girer ve Su Çıktıyı (Kabaca) Yaratır: `document.querySelector('button').addEventListener('click', () => { sayi++; targetP.textContent = sayi; } );`
Gördüğünüz gibi, Müşterinin tarayıcına İnen sey REACT Motoru değil, Sizin o HTML'nize nokta atışı yapılmış SAF Vanilla-Javascript'tir. Cihaz sıfır Yorulur.

## Kimler Kullanır?
* React'ın "Hook, useEffect, Memo, Callback" cehenneminden yorulmuş Tüm **Modern Frontend Geliştiricileri**. Piyasada Öğrenme Süresi (Learning Curve) En hızlı Olan framework'tür (HTML ve JS bilen biri 2 Saatte Svelte Öğrenir).
* Şirketlerde yavaş yavaş **SvelteKit** (Next.js'in Svelte Dünyasındaki Rakibi - SSR ve SEO canavarı) altyapısıyla Dev projeler Kurulmaya başlanmıştır (The New York Times, Spotify, vs bazi panellerinde kulllanir). Geleceğin En Parlak Yıldıızdır.
