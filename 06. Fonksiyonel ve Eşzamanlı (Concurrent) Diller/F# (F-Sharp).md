# F# (F-Sharp)

## Özet
F# (F-Sharp); Microsoft Research (Don Syme) öncülüğünde 2005'lerde geliştirilen, fonksiyonel programlamanın o muhteşem güvenlikli matematik dünyasını (OCaml altyapısını) doğrudan **Microsoft'un Devasa .NET (C#) Altyapısının** üzerine giydiren hibrit, şık ve inanılmaz veri-odaklı (Data-Driven) efsanevi bir dildir.

## Nedir ve Ne İşe Yarar?
Eğer Windows/Microsoft dünyasındaysanız C#'ın krallığında kod yazarsınız. Ancak şirketiniz bir gün "Bize öyle bir yazılım lazım ki finansal milyonlar işlem sular gibi hatasız aksın, fonksiyonel (çökmeyen) mimariyle paralel çalışsın ve aynı C++ veya Scala gibi tip zırhıyla kaplı olsun" derse C# buna tam tekil-katkısız ve saf olarak cevap veremez.

Microsoft'un bu talebe cevabı (Clojure'un Java'daki görevini .NET dünyası için üstlenen) **F#** olmuştur. F#, saf matematiksel ve fonksiyonel (Haskell / OCaml) dili gibi temizken, ihtiyaç olduğunda yan odadan C#'ın Web sunucu (ASP.NET) kütüphanelerini çağırıp onu bir saniyede tam boyutlu bir modern uygulamaya dönüştürür.

**Ne İşe Yarar?**
* **Finans, Oyun Fiziği ve Veri Analizi:** F#, verileri analiz etmek için dünyadaki en iyi veri modelleme özelliklerinden bazılarına (Discriminated Unions vb.) sahiptir. Wall-Street'teki veya Londra Menkul Kıymetler borsasındaki algoritmik ticaret ve türev risk (Quant) analizlerinin devasa C#/.NET altyapısında dondurmadan hesaplatılması için kullanılır.
* **Makine Öğrenimi (ML.NET) Uyumluluğu:** Python bu işin kralıdır, evet, ancak şirketin güvenlik departmanı "Her şey kesin statik tipli bir dilde ve .NET ortamında koşacak" diyorsa, Microsoft o AI veri modellemeleri için F#'ı en mükemmel "Fonksiyonel Ayıklama" aracı yapmıştır.

## Dilin Mantığı ve Kod Yapısı
Girinti tabanlıdır (Indentation), Python gibi tertemiz okunur, C# ve Java'nın o `}` veya `;` cehennemi yoktur.

Haskell gibi **Fonksiyoneldir**; Tipi belli değişkenleri `val` ile değiştirilemez yapar (Immutability default'tur), bu sayede eşzamanlı Thread (İplik) işlemlerinde "Aman biri sildi, kilitlendi" korkusu yaşatmaz. 

Ancak en büyük farkı **Pragmatik Hibrit** yapısıdır. Eğer isterseniz F# size "Ben tamamen Fonksiyonelim (Safım) " deyip işi yokuşa sürmek yerine "Al istersen OOP (Nesne Yönelimi - Class) kullan, ya da değişebilen değişkenler (Mutable) kullan" (örneğin UI çiziminde ihtiyaç olabilir) diye mühendise rahat serbestlik verir.

**Örnek İşleyiş (Sembolik Olarak):**
Elixir'deki "Boru Operatörü `|>` (Forward Pipe)", F#'ın ocağı tüten en ikonik buluşlarındandır. Matematiksel Listeleri veya veritabanından çekilen XML belgelerini boru hattına sokarak pürüzsüzleştirir. (İngilizce Cümle Cümle Okur gibi Veri temizlersiniz).

### Örnek Bir F# Kodu: Veri Borusu ve Discriminated Unions (Kusursuz Tip)
Finans dünyasındaki bir Müşteri Statüsünün, "C#'ın hantal Sınıf/Obje kalıtım mimarisi" olmadan F#'ta tek satırla nasıl (Tam donanım zırhlı biçimde) haritalandırıldığına ve Pipe operatörünün sihrine bakalım:

```fsharp
// F# dilinde '//' C formunda yorumdur

// DİKKAT MUCİZE 1 (Domain Driven Design - Kapsayıcı Tipler) :
// C#'da bunlar için Interface'ler Enum'lar Class'lar yazarsın yorulursun.
// F# "Discriminated Unions" ile Olasılıkları tek satıra mühürler. Çok zekidir.
type OdemeYontemi =
    | KrediKarti of string  // Yanında Kredi Karti numarasi ister
    | ApplePay            // Gerek yok, bos kalir
    | Nakit of int        // Icine pesin Nakit Tutar girilmeli

// Matematiksel ve Pürüzsüz Fonksiyon Tanimi (Parantez Eziyeti Yok)
let OdemeKabulEt (isim: string) (odeme: OdemeYontemi) =
    // Pattern Matching (Desen Eşleştirme): Erlang/Scala zekası devreye girer
    match odeme with
    | KrediKarti(kartNo) -> printfn "Sayin %s Bakiye Kredi Kartindan: %s ile Çekildi." isim kartNo
    | ApplePay           -> printfn "Sayin %s Bakiye Apple Pay ile Telefonla Temassiz Cekildi." isim
    | Nakit(pesinPara)   -> printfn "Sayin %s Bakiye %d Pesin olarak Elden Cekildi." isim pesinPara


// UYGULAMA CALISTIRMASI //
// Bu değişkenler "let" ile tanımlandığı için sonradan değiştirilemez (Immutable ve %100 Güvenli)!
let adam = "Bill Gates"
let sekli = KrediKarti("4555-0988-1233-1445")

// Sadece cagirma:
OdemeKabulEt adam sekli


// -------------------------------------------------------------
// DİKKAT MUCİZE 2: (|>) FORWARD PIPE OPERATÖRÜ (Efsanevi Veri Akışı)
// Bir finansal veri havuzunu eleyelim:

let finansalVeriler = [ 1 .. 100 ] // 1'den 100'e bir Listemiz (Veri Kümesi) Var! 

// Karmaşık iç içe geçmiş Matematik döngülerini Cümle okur gibi alt alta kuruyoruz:
let devasaSorgununSonucu =
    finansalVeriler                       // VERİYİ AL! BORUDAN AŞAĞI FIRLAT:
    |> List.filter (fun deger -> deger % 3 = 0) // Borudan akanin içinden MODU 3 olani Ayıkla(Filtrele)! Asagi Firlat ->
    |> List.map (fun kalan-> kalan * 10)    // Kalan sayilarin her birini 10 ile ÇARP (MAP)! Asagi Firlat ->
    |> List.sum                           // En sona dökülen tüm sayilari TOPLA ve Çıktıya hapset! (Sum)

printfn "Finansal İslemler Zinciri Sonu : %d" devasaSorgununSonucu
```
*Bu fonksiyonel kod silsilesi MS.NET ortamında (C# CLR yapısında) inanılmaz asenkron bir hıza denk düşer.*

## Kimler Kullanır?
* C# ve .NET Ekosisteminde kalmak zorunda olan ama Finans/Borsa Matematiği, Biyoteknoloji ya da Veri Bilimi Analizleri yaparken şirket içi kodun Asla Çökmemesi (Immutable ve Tipe Dayalı Domain Models) ve Matematik kurallarıyla yazılmasını isteyen Kurumsal Araştırmacılar.
* Yapay Zeka mühendislerinin algoritmalarını Microsoft Machine Learning (ML.NET) alt yapısında optimize eden analistler ve "Fonksiyonel-Reaktif Programlama" ile UI (Kullanıcı Arayüzü) çizdiren masaüstü Mimarları.
