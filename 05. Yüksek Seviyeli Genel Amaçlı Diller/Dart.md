# Dart

## Özet
Dart; 2011 yılında Google tarafından, ilk başlarda JavaScript'in tahtını ele geçirmek amacıyla yaratılmış ancak sonradan dünyanın en meşhur Açık Kaynaklı (Open Source) Çapraz Platform (Mobil, Web ve Masaüstü) arayüz framework'ü olan **Flutter'ın tek ve resmi dili** haline gelerek devasa bir popülarite kazanmış modern ve Nesne Yönelimli (OOP) bir dildir.

## Nedir ve Ne İşe Yarar?
2010'ların başında Google, internetteki tüm uygulamaların JavaScript spagettisine dönüşmesinden rahatsızdı. JavaScript'i öldürmek için Dart adında temiz, nesnel (Java/C# arası) bir dil yaptılar ama Chrome motorlarında JavaScript rakipsizdi. Dart projesi ölmek üzereyken 2017'de mucize oldu: Google mühendisleri **Flutter** adlı grafiksel arayüz (UI) motorunu çıkarttılar. Flutter'ın inanılmaz hızı için tek bir dile uyumlu hale getirdiler: Dart.

**Ne İşe Yarar?**
* **Tek Kodla Tüm Dünyaya (Cross-Platform):** Dart diliyle (Flutter kullanarak) tek bir kez yazdığınız tasarım/arayüz projesi saniyeler içinde doğrudan çalıştırılabilir; bir **Android uygulamasına (APK)**, bir **iOS Uygulamasına (IPA)**, aynı anda **Windows/Mac masaüstü (.exe/.dmg)** programına ve o haliyle anında **Web Sitesine** dönüşür. (Tek Kod = 4 Ayrı Platform).
* **Güzel Renderlanan (Arayüzlü) Uygulamalar:** eBay, Google Pay, BMW, Alibaba gibi dev şirketler mobil uygulamalarını ayrı ayrı Java (Android) veya Swift (iOS) yazılım ekipleriyle harcayarak 2 yılda yapmak yerine, tek bir Dart yazılım ekibiyle 6 ayda %100 aynı performanslı (Grafik motoru Skia sayesinde) Flutter ile çizer.
* **Hot Reload Sihri:** Mobil geliştiriciler on yıllarca küçük bir butonun rengini kırmızı yapmak için C++ Veya Java'nın 2-3 dakika derlemesini ekrana boş boş bakarak beklerdi. Dart (Flutter), ekranda rengi değiştirip CTRL+S yaptığınız anda milisaniyede butonu telefonda kırmızı yapar (Hot Reload).

## Dilin Mantığı ve Kod Yapısı
Tipik C ailesi dillerindendir (`{ }` süslü parantezler ve noktalı virgül `;` vardır). Ne C# kadar aşırı karmaşık kurallı ne de JavaScript kadar her şeyin birbirine girdiği tip-siz (tipi serbest) bir cehennemdir. Çok güzel bir hibrittir.

Bütün arayüzler ve her şey bir Sınıfa (Obje/Object) eşittir. **"Her Şey Bir Widget'tir"** felsefesiyle parçalar iç içe girmiş LEGO taşları (Ağaç Mimarisi) şeklinde dizilir.

Derleyici yetenekleri inanılmazdır: 
1. Uygulama geliştirirken kodlar **JIT (Just-In-Time)** derlenir, bu yüzden o an yaptığınız arayüz saniyesinde ekranda çizilir.
2. Telefonlara (Uygulama olarak) çıkmak istediğiniz asıl anda **AOT (Ahead-of-Time)** derlenir. Yani Saf Makine Diline, (Android ise ARM makine koduna, iOS ise direkt Swift klasmanında saf makine koduna) çevrilir. Bridge/Köprü sanal makine kasması (React Native vb.) olmaz! Hız C seviyesine fırlar.

**Örnek İşleyiş (Sembolik Olarak):**
Dart ile Flutter kodlarken "Ekranda orta bir kutu olsun" demezsiniz. "Center (Ortalama Objesi) Yarat", İçine "Kutu Objesi Yarat", Kutunun rengi "Kırmızı Objesi", İçine "Metin Objesi" koy şeklinde ağaç dalları kodlarsınız. Olay tamamen "Sınıf / Obje Ağacı" (Widget Tree) kurmaktır.

### Örnek Bir Dart (Flutter) Kodu: Her Şey Sınıf/Obje
Modern bir dart kodunda, Java'nın uzun uzadıya klasmanlarından kurtulmuş (Stateful/Stateless) arayüz parantezleriyle pratik bir "Mobil Uygulama Butonu" mantığı:

```dart
// Standart olarak ilk calisacak program: 'main'
void main() {
  print("Merhaba Cross-Platform Dunyasi!");

  // OOP'nin şık kullanımına çok güzel bir örnek: 
  // 'new' kelimesi modern Dart ile kaldirilmistir! Dogrudan Objeyi çagirmak yeterli.
  UygulamaArayuzu arabaModeli = UygulamaArayuzu("Elektrikli SUV", 2025);
  
  // Fonksiyon Cagirma
  arabaModeli.motorCalistir();
}

// Dart'ta bir class, C# ve Java ile neredeyse birebir ayni gramerdir:
class UygulamaArayuzu {
  // Sınıf özelliklerinde "?" Null Safety anlamına gelir, boş gelebilir.
  String isim;
  int? uretimYili; 

  // Constructor (Yapıcı Metot): 
  // Eski dillerdeki gibi `this.isim = isim;` diye tek tek eşlemeye GEREK YOKTUR!
  // Sadece parantez icinde 'this' vermek otomatik o içeriği doldurur (Söz Dizimi İncisidir).
  UygulamaArayuzu(this.isim, this.uretimYili);

  void motorCalistir() {
    // $ isareti 'String Interpolation' harikasıdır. 
    // "+" diyerek metin birleştirmeden doğrudan isim çağırırız.
    print("$isim modeli sistemi baslatti. (Yil: ${uretimYili ?? "Bilinmiyor"})");
    // "??": Eger uretim Yili NULL ise sagindaki "Bilinmiyor" stringini bas sihridir.
  }
}

// ----------------------------------------------------
// FLUTTER İÇİNDE NASIL ÇİZİLİR (WİDGET MANTIGI):
// Sadece örnek amaçlı tasarım, bu kod düz Dart'ta değil Flutter Framework ile render alınır.
/*
import 'package:flutter/material.dart';

class BenimButonum extends StatelessWidget {
  // Her şeyin (Sınıfın) Build(İnşaEt) motorundan geçtiği Lego Ağaç yapısı:
  @override
  Widget build(BuildContext context) {
    return Center(                    // Merkeze Al Objesi
      child: ElevatedButton(          // Tıklanabilir Buton Çiz Obj
        onPressed: () { 
            print("Tiklandi!");       // Anonymous (İsimsiz ok) arrow fonksiyonu
        },
        child: Text("Iceri Gir"),     // Icine Metin Ciz Objesi
      ),
    );
  }
}
*/
```

## Kimler Kullanır?
* Evrendeki milyonlarca iOS ve Android geliştiren çapraz-platform (Cross-Platform) Mobil Uygulama yazılımcıları. Freelancer'lar (Aynı anda iki dilde öğrenmek yerine tek Dart diliyle tek başlarına koca uygulamaları yayınlayan Front-end devleri).
* Kurumsal firmaların (Örn: Alibaba, Google Pay) hem Web, Hem Masaüstü (Windows) hem Uygulamadaki (Apple/Google) arayüzlerini ve kod tabanını tek bir ekiple eşitleyen (Code Reusability) Şirket mimarları.
