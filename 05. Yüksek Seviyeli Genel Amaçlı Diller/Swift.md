# Swift

## Özet
Swift; 2014 yılında Apple mühendisleri tarafından tanıtılan, kendinden önceki eski ve köklü ancak hantal Objective-C dilinin yerini alarak, Apple ekosisteminin (iOS, macOS, watchOS, tvOS) tek yetkili ve modern, çok hızlı, okuması harika olan güvenli "Resmi Cihazlar Dili"dir.

## Nedir ve Ne İşe Yarar?
Yıllarca iPhone uygulamaları `[ ]` (köşeli parantezler) ile dolu, mesajlaşma sistematğine (Smalltalk) dayanan, 1980'lerden kalma çok zor öğrenilen Objective-C dili ile yazıldı. Yazılımcılar Android tarafında tertemiz Java veya C# ile harikalar yaratırken, iOS tarafı C tabanlı bir eziyetle yoğruluyordu.

Chris Lattner (ve Apple ekibi) efsanevi C ve C++ derleyicisinin (LLVM) yaratıcısı olarak kendi elleriyle yeni bir dil inşa ettiler. **Swift**. Adı üzerinde ("Hızlı/Kırlangıç"), makine derleme hızında C++'a meydan okurken, yazdığınız okunur kod tarafında Python'un pratik şıklığını vermeyi amaçlamıştır.

**Ne İşe Yarar?**
* **Apple Ekosistemi:** Evrendeki App Store'da indirip kullandığınız WhatsApp, Instagram, Banka uygulamasının arayüzü, kameralardaki canlı oynamalar, 120Hz ekrandaki tüm "App"ler Swift ile koşturulur.
* **Sunucu Tarafı (Server-Side Swift):** O kadar güçlü ve pürüzsüz bir dildir ki (Linux üzerinde de çalışabildiği için) artık şirketler iOS uygulamalarının haberleştiği backend (sunucu/veritabanı) kısımlarını da Java veya PHP yerine (Vapor gibi kütüphanelerle) doğrudan aynı hızda Swift ile yazıyorlar.

## Dilin Mantığı ve Kod Yapısı
Swift **"Protocol-Oriented"** (Protokol Yönelimli) bir dildir. Sınıf (Class) kalıtımının Java'daki o hantal hiyerarşisinden kurtulmak için Struct (Yapı) ve Protocol'leri kilit merkezde kullanır. 

Swift'in en kilit konsepti **Optional (İsteğe Bağlı "nil" veya Değer)** tipleridir. Binlerce yazılımcının on yıllardır her programı çökerten o baş belası "Null Pointer Exception (Boş Veriye Erişme Hatası)" belasını dili yaratırken imha etmişlerdir. Örneğin, `isim` değişkeninin içi boş gelirse, Swift anında bir soru işareti (?) mekanizmasıyla bunu patlamadan engeller; uygulamanız donmaz. Uygulamanın en meşgül yerinde bir veritabanı okuyucusu bile çökmez. Ayrıca "Tip Çıkarımı" çok güçlüdür, yani değişkenin integer olup olmadığını yazmanıza gerek bırakmaz.

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin Objective-C'de metin ayıklamak isterseniz ekran `NSString *isim = @"Merhaba Dev"` diye tuhaf yıldızlı pointer sembollerine bulanırdı. Swift dilinde `let isim = "Merhaba Dev"` dersiniz. Hafiza yönetimi **ARC (Automatic Reference Counting)** teknolojisi ile çözülmüştür; sistemde bir o veriyi kimsenin izlemediğini anlasa (referans sayısı sıfırlansa) anında C++ hızında bellekten uçurur. Garbage Collector dedikleri o "geciktirici" motor Swift'te yoktur. Donanım hızındadır.

### Örnek Bir Swift Kodu: Optional, Protocol ve Şıklık
Eğer Swift'te boş (NULL) gelen verilerin uygulamamı patlatmasını istemiyorsanız ve Modern arayüzde OOP değil Protokol odaklıysanız olan biten şudur:

```swift
// Swift'te noktalı virgül (;) kullanmak opsiyoneldir (gerekmez).

// Protokol (Interface mantığı): Sadece bir yeteneğin tanımlandığı şablon sözleşmesi.
protocol IseYararMi {
    func analizEt() -> String
}

// "class" yerine Swift çok hafif ve hafıza dostu (Value Type) "struct" kullanımını teşvik eder.
struct Urun: IseYararMi {
    var ad: String
    // "?" (Optional): Bu şu demek: "Bu urunun bir puani OLMAYABILIR(NULL/NIL)". 
    // Derleyici bu satırı gördüğü an asla çökmemeye, ekstra zırh kuşanmaya mecbur kalır.
    var musteriPuani: Int? 
    
    // Protokolü kullanıp kendine uyarlıyor
    func analizEt() -> String {
        return "\(ad) detaylarina girildi."
    }
}

// 1. Durum: Puan var. 
let iphoneObjesi = Urun(ad: "iPhone 16", musteriPuani: 95)

// 2. Durum: Yeni çıkmış ve Puanı YOK (nil/null).
let yeniMacObjesi = Urun(ad: "MacBook Pro M5", musteriPuani: nil)

// SWIFT MUCİZESİ: "if let" mekanizması (Optional Binding).
// Swift derler ki: "musteriPuani" değişkeninin içi BOŞ(nil) gelebilir! 
// Seni doğrudan kullanmaya izin vermiyorum sistemi çökertebilirsin, 
// Sadece eğer içi doluysa (if let puan) o zaman bu süslü paranteze gir:
func puaniKontrolEt(urun: Urun) {
    if let kesinPuan = urun.musteriPuani {
        print("\(urun.ad) adli urunun ratingi: \(kesinPuan)") 
    } else {
        // Eğer Değer YOKSA (nil ise) SİSTEM ÇÖKMEZ (Crash Etmez), Buraya Temizce Düşer.
        print("\(urun.ad) uzerinde henuz oy verilmedi!") 
    }
}

// Foksiyonları kullan (Çıktısı hatasız, net ve temiz bir akıştır)
puaniKontrolEt(urun: iphoneObjesi)
puaniKontrolEt(urun: yeniMacObjesi)
```

Bu `if let` güvenliği olmasaydı (Örn: Eski programlarda veya C/C++ da), o `musteriPuani` değişkenini Matematiksel bir isleme sokmaya kalktığınız an sistem o ekranda milyonlarca insanın cihazını sonsuz Döngü hatası ile `CRASH` verdirerek sonlandırırdı.

## Kimler Kullanır?
* Tüm Apple "Mobile / iOS" Developer ordusu. Şirketlerin telefonlarımıza indirdiği devasal oyun ve arayüz programlamalarının yapımcıları.
* macOS işletim sistemi (M1, M2 çipli işlemcilerin optimizasyonu için en kusursuz araçtır) üzerinde App Store'da ofis donanımları geliştiren masaüstü mühendisleri.
* Veritabanı yönetimli veya web tabanlı uygulamaların saf iOS portunu native yazmak (Örn: Flutter/React Native yavaşlığından kaçmak) isteyenler.
