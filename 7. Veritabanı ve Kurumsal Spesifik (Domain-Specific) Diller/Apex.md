# Apex

## Özet
Apex (Salesforce Apex); 2006 yılında dünyanın en devasa kurumsal Müşteri İlişkileri Yönetimi (CRM - Customer Relationship Management) bulut şirketi olan **Salesforce** tarafından icat edilen, şirketlerin kendi özel bulut (Cloud) platformları üzerine e-ticaret/satış algoritmalarını kodlamaları için geliştirilen, temel olarak **Java dilinin %90 oranında birebir aynısı olan (Klonlanmış)** ancak "Bulutta Veritabanı Sınırlandırmaları" barındıran katı ve tam-teşekküllü (Nesne Tipli - OOP) spesifik bir programlama dilidir.

## Nedir ve Ne İşe Yarar?
Eskiden firmalar müşteri veri bankasını bilgisayarlarındaki Excel'de tutardı. Sonra Amerikalı Salesforce geldi ve "Verilerinizi bize verin (Buluta verin), biz onları size Grafik Arayüz (CRM) olarak göstereceğiz, temsilcileriniz oraya tıklayıp müşteri takibi yapsın!" dedi. Salesforce dünyadaki en büyük Cloud SaaS sistemi oldu.

Ama bir noktada "Ferrari (Mesela otomobil bayi) firması" dedi ki; "Müşteri listesinde Ferrariyi alan kişi, eğer Kırmızı renkliyi secdiyse ona ekstra 5.000 Dolar prim vergisi çıkaran Butonu nasıl kodlayacağım?". Salesforce'un menü arayüzü ve sürükle/bırakan otomasyonları (Salesforce Flows) patlıyor, yetişmiyordu. Çözüm olarak **Apex dili** icat edildi! Siz bir `*.cls (Class dosyası)` yazarsınız, onu Salesforce Bulutuna itersiniz ve kodunuz şirketin milyarlarca dolarlık Müşteri Kayıt (Süreci) platformunun kalbinde, Bulutta çalışır.

**Ne İşe Yarar?**
* **Müşteri İlişkileri (CRM) Otomasyonları:** Eğer bir çağrı merkezindeki Müşteri (Account) Kaydının "Yıllık Geliri 1 Milyonu aştıysa"; O saniyede Bulutta (Server-Sided) otomatik Apex tetikleyicisi uyanır (Apex Trigger) ve Satış Temsilcilerine "Bu adam VIP oldu, çiçek yollayın!" maili atar. B2B ticaret krallığının otomasyon fişidir.
* **Force.com Gücünü Özelleştirme:** Salesforce altyapısında çalışacak Frontend (Eskiden Visualforce, şimdi LWC - Lightning Web Components) JavaScript (Vue/React klonları) menülerinin, Arka plan (Backend/Controller) verilerini çekme aracı sadece ve sadece Apex'dir.

## Dilin Mantığı ve Kod Yapısı
Apex'in sözdizimi, **Java'nın bir çatalı (Fork)** gibidir. Java (veya C#) biliyorsanız Apex öğrenmeniz sadece yarım saatinizi alır (Süslü parantezler, `public static void`, `List<String>`). Lakin çok ölümcül farklı KISITLARI vardır:

1. **Multitenant (Çoklu-Kiracı) Kalkanı - ÇILDIRTAN LİMİTLER (Governor Limits):** Siz kodunuzu kendi bilgisayarınızda değil, Binlerce diğer şirketle (Müşteriyle) cùng bir Bulut Ortak-İşlemcisinde (Salesforce Pod) çalıştırırsınız. Bu yüzden Apex size şöyle bir kural koyar: "Eğer döngünün içinde veritabanı sorgusu (SOQL) atarsan, veya kodun X saniyeyi aşarsa Kodunu (İPİNİ) acımadan Kılıçla Keserim (Exception fırlatarak sistemi öldürürüm)". 1 Kodda maksimum 100 kere Veritabanı sorgu Hakkınız vardır. O yüzden kodunuzu Toplu(Bulkified) yazmak zorundasınız.
2. **Kendi SQL'ini İçinde Taşır (SOQL):** PL/SQL'deki gibi SQL'i string (metin `"` `"` ) olarak yazmazsınız. Direkt Java listesine iter gibi `List<Contact> kisiler = [SELECT Id FROM Contact]` diyerek Java kodunun ta içine Köşeli Parantez `[]` ile Database dilini (SOQL - Salesforce Object Query Language) GÖMEBİLİRSİNİZ.

### Örnek Bir Apex Kodu: 100 Tane Müşteriyi (Account) Aynı Anda Bulup VIP Yapmak (Bulkification Zekası)
Java'ya birebir benzemesine rağmen İçinde Gömülü (Köşeli Parantezli) SQL dili barındıran ve Hata yaparsanız Bulutun derhal fişi çekeceği (Döngü dışı DB Mimarisi):

```java
// Apex'de (Tıpkı Java'daki) "//" ve "/* */" yorumlari kullanılır

// Bir "MusteriIslemleri" class'i, public erisimle Salesforce Cloud'a doguyor!
public class MusteriIslemleri {

    // Apex'de deklare edilen Ana Metotlar (Eger Trigger/LWC den cagirilacaksa):
    public static void yeniMusterileriVIPYap(List<Id> yollananMusteriIdListesi) {
        
        // 1. DÖNGÜ İÇİ YASAKLANAN SİMÜLASYON! (Governor Limits)
        /* ASLA BUNU YAPMA: (C#'ci gelir gelmez boyle yazar): 
           for(Id id : yollananMusteriIdListesi) { 
               Account acc = [SELECT Status FROM Account WHERE Id = :id]; // LIMIT ERROR!
           }
        */

        // 2. DOĞRUSU: BULKIFICATION (TOPTANCI ALIMI VE KÖŞELİ SOQL (!))
        // Diyoruz ki: Yollanan ID Listesindeki Bütün Müşterileri TEK SEFERDE SQL(SOQL) ile Listeye cek!
        // DİKKAT: Apex'te String yazmadan, Sınıfın ta ıçıne [SELECT ...] yazılabılır (Inline SOQL)
        // ':' (iki nokta), Apex degiskenini Database kodunun icine(Bind Var) Sokar!
        List<Account> incelenecekMusteriler = [
            SELECT Id, Name, VIP_Status__c, AnnualRevenue 
            FROM Account 
            WHERE Id IN :yollananMusteriIdListesi
        ];

        // Guncellenecek Müşterileri atacağımız BOS BIR TOPTAN LİSTE Yaratiyoruz (Set/List):
        List<Account> guncellenecekVeritabanıListesi = new List<Account>();

        // 3. EKRANDAN ÇEKİLEN VERİLERİN ÜZERİNDE GEZ (OOP / Java mantığı For-Each)
        for (Account iterasyonMusteri : incelenecekMusteriler) {
            
            // Eger Musterinin 'Yillik Geliri' 1 Milyin'u aşıyorsa
            // (Salesforce'da özel oluşturulan verigaba Sütunlari _c ile biter(Custom!))
            if (iterasyonMusteri.AnnualRevenue > 1000000) {
                
                // Müşteri Statusünü VIP'ye Cek! YILDIZLA!
                iterasyonMusteri.VIP_Status__c = 'PLATINIUM VIP';
                
                // Ve hemen bu güncel müşteriyi "Guncellenecekler Torbasına" at
                guncellenecekVeritabanıListesi.add(iterasyonMusteri);
            }
        }

        // 4. DATABASEE YEDIR (DML - Data Manipulation Language Zirvesi)
        // Eğer guncellencek torba 0'dan buyukse (Empty degilse)...
        if (!guncellenecekVeritabanıListesi.isEmpty()) {
            
            // "update" Bir API komutu degil, dogrudan DİLDEKİ BIR ANAHTAR KELİMEDİR! (PL/SQL gibi)
            // Biz Update torba dedigimizde; Apex arkaplanda o torbadaki 50 tane Müşteriyi 
            // Database'e 'Tek bir istek atarak' isler.
            try {
                update guncellenecekVeritabanıListesi;
                System.debug('Sistem Mesaji (Console Log): Musteriler Basariyla VIP yapildi!');
            } catch (DmlException e) {
                // Kaydederken Bir "Dogrulama/Eksik Doldurma Hatasi" Cikarsa Konsola(Log) Bas!
                System.debug('BİR FELAKET ÇIKTI, Database Onaylamadi: ' + e.getMessage());
            }

        }
    }
}
```

Apex programcıları Java kodunun asilliğiyle Database sorgusunun o keskin bıçağını sürekli aynı Class (Sınıf) içinde harmanlayan Toptancı esnafı gibi (Bulkuification) yaşarlar.

## Kimler Kullanır?
* Evrendeki sadece ve sadece **Salesforce Geliştiricileri (Salesforce Developers)**.
* Bu dil, kapalı kapılar arkasındaki bir Krallıktır. Bilgisayarınızda (Localserver) çalıştıramazsınız. Bir Compiler'ı yoktur, yazdığınız kod, internet yoluyla Amerika'daki Salesforce sunucusuna gider, orada derlenir, hata varsa size hatayı internetten yollar. 
* O kadar değerlidir, Kısır ve (Niş) bir piyasadır ki; bir şirkete (Vodafone, Ford vb) Salesforce'u entegre edebilecek usta "Apex Coder" (Sertifikalı PD1/PD2 uzmanları) silikon vadisi devleri (Google JS Devs) haricinde dünyada dolarla en yüksek maaşı/Consulting ücretini alan danışman tayfasıdır.
