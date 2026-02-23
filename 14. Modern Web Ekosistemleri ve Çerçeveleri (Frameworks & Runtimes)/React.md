# React

## Özet
React (veya React.js); 2013 yılında **Facebook (Meta)** Mühendisi Jordan Walke tarafından icat edilen, İnternet sayfalarındaki Tuşların/Kutuların (Bileşenlerin / Components) birbirine karışmasını engelleyen ve Veriyi anında (Sayfayı yenilemeden) ekranda Işık Hızıyla senkronize eden, Dünyanın en devasa, en popüler, Tek-Sayfa Açılımlı (SPA - Single Page Application) Ön-Yüz (Front-End) **UI Kütüphanesidir**. (Kendisi bir tam teşekküllü MVC Framework değildir lakin öyleymişcesine muammele görür).

## Nedir ve Ne İşe Yarar?
Eski zamanlarda Facebook'a bir Fotoğraf yüklediğinizde veya Biri mesaj attığında Onu (Bildirim Sayacını Kırmızı *1*) yapabilmek için "Sayfayı F5 atıp Tüm Web Sitesini baştan (Yenilemeniz / Reload) Yüklemeniz" gerekiyordu. jQuery veya eski JS ile Ekrandaki Düğmelerin İçinde yatan Cümleleri (`innerHTML`) değiştirmek sayfayı kasıyor ve kodları çorbaya çeviriyordu (Amelelikti). Spagetti kodu doğuyordu.

Facebook Mühendisleri dedi ki: "Bizim Ana Bir Verimiz (State - Durum) olsun. (Mesaj Sayısı: 0). HTML kodlarını Kutu Kutu (Component - Bileşen) olarak parçalayalım (Header Ayrı, MesajKutusu Ayrı). Eğer State *5* Olursa, BİZ HTML'ye ELLEMEYELİM; Arka plandaki **Yalancı-Hayalet İskelet (Virtual DOM) motoru** o değişikliği hissetsin, sayfa yenilenmeden Ekranda SADECE o köşedeki Kırmızı O Tuşun Rakamını 5 yapsın! Diğer hiçbir şeye dokunmasın". Bu felsefeye Component State Mimarisi denir ve **SPA (Single Page Application)** dönemi, "Hiç F5 yenilememe (Masaüstü EXE programı)" dönemi başlamıştır.

**Ne İşe Yarar?**
* **Dinamik, Kompleks ve Reaktif UI Çizimi:** Instagram (Web), Netflix, Twitter (X) gibi sayfayı asla yenilemeden aşağıya kaydırdıkça(Scroll) yeni verileri arkaplandan (Fetch/Axios) anında çekip pürüzsüz animasyonlarla (DOM çökmeden) ekrana basan modern internet mimarileridir.
* **Component (Lego) Mimarisi Sayesinde Kod Tekrarsızlığı:** Bir "Özel Siyah Login (Giriş) Butonu" Tasarlarsınız (Kodu 200 Satırdır). İlerleyen günlerde o butonu Web Sitersinin başka 5 Ayrı sayfasında kullanmak istediğinizde Kopya Yapıştır İğrençliği YAPMAZSINIZ! ` <OzelSiyahLoginBtn/>  ` diyerek (Kendi yaptığınız HTML Kılıklı Etiketi) oraya enjekte edersiniz. 

## Dilin Mantığı ve Kod Yapısı
Tamamen **JavaScript (ES6+)** veya **TypeScript** yazılır. Ama en büyük ve Korkunç Buluşu: **JSX (JavaScript XML)** Tır.

Eskiden Front-end Mimarisi Şöyle derdi: "HTML iskeletin, CSS Makyajın, JS Sinir Sistemindir, Hepsini ayrı doymalarda Ayrı Ayrı Tut" (Evrensel kural buydu).
React dedi ki: "**HAYIR! Bir Ekrana Çizilecek Tuş Tıklanabilir (JS) degilse ÖLÜDÜR. O Halde KODU VE HTML Tasarımını Aynı JavaScript Dosyası (Function) İçine Gömüyorum!**"

JSX Sayesinde, JavaScript Kodlarının Ta kalbine / Dönüş (Return) Emirlerine Doğrudan Dümdüz Html Çakılır ( `<div/>` gibi). Bu başlarda C++ yazılımcılarına "Lanet Tıklanmalık Bir Küfür (İğrençlik)" gelmiş, sonradan "Veri Ekranla Nasıl Bu Kadar Pratik Birleşebillir" diyerek Tapanları trilyonlarca artırmıştır.

**Örnek İşleyiş (Eski vs React):**
Eski JS: `document.getElementById('mesajKutu').innerText = "Ali";`
Modern React: State değişir (`isim = 'Ali'`). HTML Ekranda: `<p> Hosgeldin {isim} </p>` Olarak otomatik Şişer (Reaktiflik / Tepkisellik).

### Örnek Bir React (.jsx / .tsx) Kodu: Tıklanınca Artan Sayaç (Counter) Komponenti!
JavaScript dosyasının içerisinde; HTML Dönen (JSX) ve Farenin tıklamasıyla Ekranda "1...2..3.." diye Saniyesinde (Virtual Dom'da) şişen fonksiyonel Komponent (Hooks Mantığı):

```jsx
/* BU BIR REACT.JS KODUDUR (Javascript İcinde Saklanan HTML/JSX Mucizesi) */

// React Cekirdeginden (NPM Modullerinden) "useState (Hafizada Durum Tutma)" Kancasını Çek!
import React, { useState } from 'react';

// 1. COMPONENT (BILESEN) YARATIMI: Klasik bir Fonksiyondur ama Bas harfi Büyüktür!
// Bu Fonksiyon cagirilicna Ekrana (DOM'a) HTML Parcalari Kusar(Return).
function SayacButonuMenu() {
    
    // 2. STATE (CANLI/REAKTIF VERİ) YARATIMI: Memory/Hafiza!
    // tiklamaSayisi: Su anki Guncel Rakam (Baslangic: 0)
    // setTiklamaSayisi: Bu rakamı İlerleyen Saniyelerde Degiştirecek "Emir/Komut"
    const [tiklamaSayisi, setTiklamaSayisi] = useState(0);

    
    // 3. JAVASCRIPT LOGIC (MANTIK) ISLEMI (Ok Fonksiyni)
    const kutuTiklandiginda = () => {
        // Hafızadaki rakami (Eski var olanin uzerine +1 Ekleyip) Guncelle/Ekrana At!
        setTiklamaSayisi(tiklamaSayisi + 1);
    };

    
    // 4. MUCİZE: RETURN EDILECEK OLAN (SİMBİYOTİK) JSX = HTML + JAVASCRIPT KARSIMI!
    return (
        // Javascript'in (return) komutu icinde String veya Tırnak OLMADAN html etiki basliyor: 
        <div style={{ padding: '20px', border: '1px solid gray' }}>
            
            <h1>Sihirli Reaktif Counter(Sayac)</h1>
            
            {/* JSX Icine Kirvircik Parantez { } İle Canli JavasCript Degiskenlerini Zerk Ederiz: */}
            <p>Butona Su Ana Kadar TAM OLARAK: {tiklamaSayisi} Kere Bastiniz.</p>
            
            {/* HTML EVENT (onClick) 'ine Yukarida Yarattigimiz JS Fonksiyonunu (kutuTiklanginida) YAPIŞTIR */}
            <button onClick={kutuTiklandiginda}>
                Beni Ziplat (Sayaci Arttir)
            </button>
            
        </div>
    );
}

// Baska Componentlerde (Orn: Ana Sayfa index.js 'de) <SayacButonuMenu /> sekline lego gibi kullanmak icin İhrac et!
export default SayacButonuMenu;
```

Bu Kod Chrome'da çalıştığında; Button'a Bastığınız an *Sayfa Yenilenme (Loading) İkonu* ASLA Dönmez. Sayfanın başındaki Navbar'daki Resminiz Pırıldamaz. Sadece `<p> 0 </p>` sayısı `1` olur (Milisaniyenin 10'da biri O VirtualDom Tarafından sezilip boyanır). Bu Yüzden Facebook 2 Trilyon Mesaj trafiğiyle asılı kalmaz. Akış (Flow) kusursuzdur.

## Kimler Kullanır?
* Evrendeki ve Silikon Vadisindeki **Web (Front-End) / Single-Page Application (SPA)** İlanlarının %70'inden fazlasını domine eden **React Developer**'lar (Milyonlarca Frontend Geliştirici).
* Native Mobil Geliştirmeyi bitiren **React Native** Sayesinde; Cep telefonundaki (Android/iOS) Aynı mantıkla (Ufak değişimlerle) mobil uygulama çıkaran Cross-Platform Yazılımcıları.
* Günümüzde doğrudan saf React (Vite/CRA) Kurulumu, SEO (Arama Motoru indeksleme) dezavatajı yarattığı için **Next.js** Kapsülü içinde Ana omurga (Rendering) olarak kullanılmaya Evrimleşmiştir (Levelin Sıradaki Dilidir).
