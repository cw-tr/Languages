# ECMAScript

## Özet
ECMAScript (ES); 1997 yılında Ecma International kurumu (Zürih) tarafından standartlaştırılan, **JavaScript** (ve ActionScript, JScript gibi diğer dillerin) temel çekirdeğini, kurallarını ve sözdizimini (Syntax) belirleyen Evrensel ve Resmi **"Programlama Dili Spesifikasyonu (Standartlar Kitabı)"** dur. Javascript bir marka ve motor iken; ECMAScript o motorun nasıl çalışması gerektiğini yazan Yasal Anayasadır.

## Nedir ve Ne İşe Yarar?
1995'te Netscape firması Tarayıcıya Hareket katmak için "Mocha" (Daha sonra "JavaScript") dilini 10 günde icat ettiğinde, her şey çok dağınıktı. Microsoft hemen kodları çalıp(Reverse Engineer) Internet Explorer için kendi "JScript" versiyonunu çıkardı. İki tarayıcının kodları (Aynı JS gibi görünse de) birbiriyle uyumsuz çalışarak ortalığı cehenneme çevirdi (Browser Savaşları: Aynı siteniz Biri açılır ötekinde Patlardı).

Netscape panikle İsviçre'deki Ecma kurumuna gitti ve "Bunu standartlaştıralım! Bütün şirketler şu anayasaya uymak zorunda kalsın" dedi. **ECMAScript (ES)** doğdu!

**Ne İşe Yarar?**
* **JavaScript'in Akıl Hocasıdır:** Bugün biz kodlarken "Ben JavaScript yazıyorum" deriz ama aslında %100 "ECMAScript standardını uygulayan" bir yazılım geliştiriyoruzdur.
* **Modern Evrimi Temsil Eder (ES6 Devrimi):** 2015 yılında çıkan **"ECMAScript 2015" (Nam-ı diğer ES6)**, Javascript tarihinin en büyük değişimiydi. (`var` belasından kurtulup `let/const`'a geçiş, `=>` (Arrow Function)ların gelişi, `class` mantığının gelişi). O günden beri her yıl ES7, ES8 (Örn: `async/await`), ES9 gibi güncellemeler (Yıllık standartlar) çıkmaya devam eder.

## Dilin Mantığı ve Kod Yapısı
ECMAScript kendisi çalıştırılabilir (Indirilip Kurulan Exe) BIR ŞEY DEĞİLDİR; sadece devasa bir PDF (Doküman) tır. 

Google (V8 Engine) veya Mozilla (SpiderMonkey Engine), ECMAScript'in yayınladığı bu PDF'i (Örn: Diyor ki, `array.includes()` metodu olmalı) okurlar ve kendi C++ Browser(Tarayıcı) motorlarına bu özelliği O Yıl içerisinde inşa ederler (Implementasyon). Sizin Javascriptiniz Chromium sayesinde çalışır.

**Örnek İşleyiş (Evrim / ES Farkları):**
**ES5 Öncesi (Karanlık Çağ Javascripti):**
`var x = 10; function topla(a) { return a + x; }`
**(2015 Devrimi) ES6 (Modern Javascript Standardı):**
`const x = 10; const topla = (a) => a + x;` (TypeScript veya CoffeeScript özentisi, muazzam modern yapı).

### Örnek Bir ECMAScript Standart Gelişimi: Async/Await (ES8 - 2017) ve Classlar (ES6)
Eski Javascript'te Veritabanından (Veya API'den) veri Çekerken .then() ve Callback(Cehennemi) icinde Bogulurduk. ECMA komitesi karar alıp 2017'de (ES8 ile) dile "Bekleme(Await)" standardı ekledi:

```javascript
/* MODERN ECMASCRIPT (ES6+ ve UZERI) KODLAMASI ! */

// 1. ES6 Sınıf(Class) Standardı (Eski Prototype ameleliginin cokuşü!)
class UzayGemisi {
    
    // Kurucu metot (Yine ES6 Standarti)
    constructor(isim) {
        this.isim = isim; 
    }
}

// 2. LET ve CONST (ES6) - "var" Kelimesinin Tarihe Gomulusü
// `var` kullanimi tum Javascript dosyasinda (Global) sizarak bug yapirdi. 
// ECMA komitesi Scope (Kapsam) tabanli guvenli "let ve const'u" getirdi:
const gemim = new UzayGemisi("Apollo"); 


// 3. ASYNC / AWAIT KARDESLIGI (ES8 - 2017 GÜNCELLEMESİ)
// Bir verinin internetten Inmesini (Milisaniyeler) beklemek icin kullanilan Modern Standart
const API_VeriCek = async () => {   // Ok Isareti (Arrow Function -> ES6)
    
    try {
        // 'await' Kelimesi kodu asagi salmaz. Inmesini Bekler (Sleep gibi deil C# Taski gibi)
        const yanit = await fetch('https://api.uzaycilar.com/data'); 
        
        // Gelen Datayi JSON Formatina Cikar
        const jsonVeri = await yanit.json();
        
        // 4. ES6 TEMPLATE LITERALS (Backtick `` ve Dolar Susulusu Eklentisi)
        // Eskiden: console.log("Gelen Veri " + jsonVeri.isim + " dir"); Diye eziyet edilirdi:
        console.log(`Gelen Veri ${jsonVeri.isim} Dir! (Modern String)`);
        
    } catch (hata) {
        console.error("Hata Dustu:", hata);
    }
}
```
Buradaki tüm yenilikler (`class`, `const`, `=>`, `async`, `$()`) spesifik olarak "ECMAScript Yürütme Komitesi (TC39)" tarafından toplantılarla yıllarca oylanıp dile resmi eklenti olarak dahil edilen Evrensel standatlardır.

## Kimler Kullanır?
* Evrendeki bütün **Tarayıcı Motoru Üreticileri (Google V8, Mozilla, Apple WebKit)**, bu standartname'yi (Ecma-262) alıp her yıl güncellenen JS Engine C++ kodlarını yazan Mühendislerdir.
* **Tüm JavaScript/Front-End Geliştiricileri**; iş ilanlarında ("Javascript arıyoruz" yerine) sık sık "ES6 (Modern JS) teknolojilerine hakim geliştirici" terimini görür. Geliştiriciler (Babel gibi Transpilerlar sayesinde) ES9/ES10 kodlarını yazar ve Chrome'un onu çalıştırabilmesine güvenir.
