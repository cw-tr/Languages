# TypeScript (TS)

## Özet
TypeScript (TS); 2012 yılında Microsoft (ve Anders Hejlsberg, C#'ın yaratıcısı) tarafından geliştirilen, JavaScript dilinin o kaotik "Zayıf, Tipsiz ve Kuralsız" yapısını tıpkı bir Java veya C++ kadar disiplinli, Milyon Satırlık (Kurumsal) Katı OOP projelerine dönüştüren, nihayetindeyse kodunu tekrar Makine diline değil "Saf JavaScript'e (Derleyen/Transpile eden)" süper bir JS üst-kümesidir (Superset).

## Nedir ve Ne İşe Yarar?
JavaScript ile bir hesap makinesi veya haber sitesinin kayar panosunu (Slider) yazmak kolaydır. 150 Satır sürer. Ancak Facebook, Microsoft Teams veya VS Code gibi 5 Milyon satırlık devasa bir proje yazdığınızda, binlerce farklı çalışanın yazdığı binlerce fonksiyon birbirine girer. 

Örneğin JavaScript'te siz `kullanici_yarat(kisi)` diye fonksiyon yazarsınız, başka bir yazılımcı yanlışlıkla o fonksiyona Kişi Objesi (`{ad:"Ali"}`) yerine Dizi `[1,2,3]` yollar. JavaScript asla hata fırlatmaz, sessizce o veriyi çalıştırır ve "Undefined" (Bilinmiyor) diyerek milyarlarca dolarlık ödeme veritabanını darmadağın eder. O hatanın Nerede (Hangi 3 milyonuncu satırda) olduğunu program CANLIYA (yayına) geçene kadar Ruhunuz Bile Duymaz (Runtime Error).

Microsoft (Anders), TS'yi icat etti. "Biz JS'ye **Tip (Type)** kuralı, C# Sınıfı (Interface/Generics) mimarisi getirelim. Programcı kodu yazarken (Derleme/Compile sırasında) daha hatayı yaptığı MİLİSANİYE içinde kelimenin altını Kırmızı Çizgiyle uyarsın (Tip Hatası) ve bu güvenlikle (Compile Time Error) dev sistemleri yönetelim". Ancak tarayıcılar (Google Chrome) TS Dilini bilmez. O yüzden yazdığınız harika, zırhlı TypeScript kodu, yayımlama esnasında sıfır hatayla "Düz Güvenli JS'e" dönüştürülür ve dünyaya öyle dağıtılır (Transpiler).

**Ne İşe Yarar?**
* **Devasa Kurumsal Ön-Yüzler (Enterprise Frontend):** Bugün React, Angular veya Vue projelerinin profesyonel ortamda (Bankalar, Finans sistemleri, Dev Yazılım Ekipleri) geliştirilen %95'i JavaScript ile DEĞİL, zorunlu olarak TypeScript ile kodlanır. Hatayı sonradan değil kodlarken yakalamak, dev şirketlerin milyar dolarını kurtarır.
* **Akıllı IDE / Otomasyon Yardımı:** Yazdığınız her şeyin ne olduğunu C# gibi mühürlediğiniz (Tiplendirdiğiniz) için, Visual Studio Code size siz kod yazarken Otomatik Tamamlama (Autocompletion) penceresinde her şeyin parametresini muazzam bir zekayla sunar (IntelliSense).

## Dilin Mantığı ve Kod Yapısı
Sözdizimi JS ile BİREBİR aynıdır, sadece **`iki nokta ve ardından Tip Adı (: string)`** kuralı vardır. Normal geçerli bir JS kodu zaten geçerli bir TS kodudur ("Typescript is a superset of JS").

TS'de Interface'ler (Arayüzler Şablonu) ve Type Alias (Tip kısaltmaları) inanılmaz derecede esnektir. O yüzden "Java gibi kasıntı" değil, "Pürüzsüz ve Yapısal" bir OOP'dir (Structural Typing - Duck Typing). Bir şeye "Kuş" diyorsanız ama kanadı yoksa TS sizi anında kilitler ve Compile(Derlemeyi) hata verip reddeder.

**Örnek İşleyiş (Sembolik Olarak):**
JS'de: `function harca(Miktar) { return Miktar * 2 }` -> Biri Miktar yerine "Elma" yazarsa program `NaN` (Sayı Değil) hatasına bulanarak çalışmaya devam eder, kilitlenir.
TS'de: `function harca(Miktar: number): number { ... }` -> Biri "Elma" stringini yazmaya kalktığı o ilk an, Satır kırmızıya döner ve yazılımcıya "Verilen argüman 'number' türüne uygulanamaz" tokatı atar.

### Örnek Bir TypeScript Kodu: Arayüz (Interface) Zırhı ile Hatasız Front-end
Görünümü Java veya C#'ı andıran, ancak en nihayetinde (çıktı alındığında) o Tip kalkanlarının uçup saf tertemiz JS'e dönüştürüldüğü Modern/Kurumsal TypeScript (React benzeri API Veri Katmanı) Mimarisi:

```typescript
// === TS MUCİZESİ: "INTERFACE (ARAYÜZ ZIRHI)" Tipi ===
// Uygulamamızda Milyonlarca "Ürün / Product" objesi olacak. 
// Bunlar JS'deki gibi başı boş kalmasın diyerek "Tam Kalıbını/Şemasını" dayatıyoruz:

interface UrunSemasi {
    id: number;                 // Sadece Rakam olabilir
    kategori_adi: string;       // Sadece Metin
    stok_miktari: number;
    kampanyali_mi?: boolean;    // '?' Soru işareti var, yani Indirimli olmayabilir (Opsiyonel / undefined dönebilir)
}


// === 2. ADIM: KATİ/SATI TİPLİ FONKSİYON YAZMAK ===
// API'den bir ürün alacaksın, ama bana "Kesinlikle" ve "Sadece" UrunSemasi zırhına uygun 
// objelerin olduğu bir Dizi (Array '[]') dondüreceksin diye emir veriyoruz!:

async function veriTabanindanUrunleriGetir(api_url: string): Promise<UrunSemasi[]> {
    
    console.log(`${api_url} Adresinden veri iniyor...`);
    
    // Farz edelim ki Sunucudan veri çektik ve Objeye (JSON) çevirdik
    // JS Bize bunu "Any/Bilinmeyen" olarak fırlatır. Lakin TS devreye girip kalıba döker.
    
    const veritabanindan_gelen_veri: UrunSemasi[] = [
        { id: 101, kategori_adi: "Kitap",  stok_miktari: 54, kampanyali_mi: true },
        { id: 102, kategori_adi: "Laptop", stok_miktari: 11  } // Bunda? Opsiyonel olduğu icin Çökmez!
    ];
    
    /* 
    !! HATALI KOD DENEMESİ (Eğer Alt satırı aktif etseydik TS bizi MAHVEDERDİ, Derlemezdi):
       { id: "KALEM_10", kategori_adi: "Kirtasiye" } 
       -> TS HATASI: "Type 'string' is not assignable to type 'number' for 'id'." 
    */
    
    return veritabanindan_gelen_veri; 
}


// === EKRANA (UI) YAZDIRMA İŞLEMİ ===
// JS'in meşhur Ok(Arrow ()=> ) fonksiyonlarıyla, ancak Typescript kurallarında:

const vitrine_urun_ciz = (urunler: UrunSemasi[]): void => {
    
    urunler.forEach((urun) => {
        // Otomatik Tamamlama Gücü: Editörde "urun." yazdiğiniz an, 
        // VSC (Editör) size sadece ve sadece "id, kategori_adi, stok vs" gosterir.
        
        console.log(`Vitrin Kodu [${urun.id}] olan ${urun.kategori_adi} | Stok Cekmecesi: ${urun.stok_miktari}`);
        
        // Mantiksal (Güvenli) Kontrol, (TypeScript Bool/Kararları Kusursuz Bikir)
        if (urun.kampanyali_mi) {
            console.log("-> SATIS ETIKETI: 1 ALANA 1 BEDAVA!");
        }
    });
};

// Asil Uygulamayi calıştıran tetik:
veriTabanindanUrunleriGetir("https://shop.com/api/v1/products")
    .then((liste) => vitrine_urun_ciz(liste));
```
*(Bu kod çalışmaya / browser'a gitmeye hazırlandığında "tsc (TypeScript Compiler)" sayesinde içindeki bütün `number`, `string`, `interface` kelimeleri Jilet gibi silinir ve geriye çok güvenli / hata ayıklanmış o efsanevi çıplak Javascript mantığı kalır).*

## Kimler Kullanır?
* Angular (Zaten direk TS ile yazılmıştır), React.js ve Vue.js ile devasa, şirket-içi (Enterprise), 10.000+ dosyalı yazılım modülleri üreten ve hata payını sıfırlamak isteyen Büyük Front-end (Ön yüz / Web UI) Geliştirici Orduları.
* Node.js (Sunucu / Backend) geliştirirken JS'in tanımsız mimarisinden cayıp "Sanki C# ile Sunucu Yazıyormuş" kalite, güvenlik ve Interface standartlarına ulaşmak isteyen Full-Stack Yazılım Mimarları.
