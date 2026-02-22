# Move

## Özet
Move; aslen Facebook (Meta) mühendisleri tarafından (iptal edilen "Diem/Libra" kripto para projesi için) 2019 yılında icat edilen, Solidity'nin güvenlik açıklarını silip atmak için **"Parayı (Tokenları) Bir Rakam (Int) Olarak Değil, Kopyalanamayan ve Silinemeyen Fiziksel Bir Obje (Resource)"** olarak modellediği, günümüzde Aptos ve Sui blokzincirlerinin ana dili olan ve tarihin en güvenli Akıllı Kontrat dillerinden biri kabul edilen Rust-tabanlı Web3 programlama dilidir.

## Nedir ve Ne İşe Yarar?
Solidity (Ethereum) ile kod yazarken bir kullanıcının bakiyesini tutmak için `mapping(address => uint256)` yazardınız. Yani para sadece Veritabanındaki "100" yazan bir değişkendi. Eğer hacker kodunuzdaki bir açığı (Reentrancy attack) bulup o "100" rakamını eksiye (-) düşürmeden kendine kopyalamayı başarırsa sahte para basar ve sistemi soyardı.

Move Dili Dediki: **Kripto para bir SAYI Olamaz, Fiziksel Bir Eşya (Kaynak/Resource) Olmalıdır!**
Oyun dünyasındaki "Kılıç" eşyası gibidir. Bir kılıcınız varsa, onu aynı anda hem Ali'ye hem Ayşe'ye veremezsiniz. Onu Çoğaltamazsınız. Onu çöpe atamazsınız. Onu sadece "Adresten Adrese" fiziksel bir Kargo gibi Taşıyabilirsiniz (İsmi buradan Move gelir).

**Ne İşe Yarar?**
* **Aşırı Güvenli DeFi Protokolleri:** Aptos ve SUI ağları saniyede 120.000 işlem (TPS) yapmasına rağmen, Move dili sayesinde "Aynı parayı iki kere harcama (Double Spending)" ve "Sonsuz para basma" gibi Hack olaylarını Derleyici (Compiler) seviyesinde matematiksel olarak Engeller. Hacker istese de öyle bir kod derlenmez(Bytecode Verification).
* **Paralel İşleme (Parallel Execution):** Eski EVM'ler (Ethereum) işlemleri tek tek sırayla çozmek zorundadır. Move ise eşyalar (Resources) birbirinden ayrı izole kutularda olduğu için 10.000 farklı para transferini Aynı Saniyede(Çok çekirdekli Cpu ile) çalıştırarak ışık hızına ulaşır.

## Dilin Mantığı ve Kod Yapısı
Move dili altyapı olarak **Rust** dilinden mükemmel esinlenmeler taşır. 
Rust'ın meşhur (Ve Çok sinir bozucu) olan **Sahiplik (Ownership) ve Ödünç Alma (Borrowing)** kavgası; Move'un kalbidir. 
Eğer Ali bir "Altın Coin" yaratırsa O Coin Ali'nindir. O Coin'i harcadığı an, Bellekten (O Fonksiyondan) Çıkarılır. İki kere harcama komutu yazarşanız Derleyici HATA fırlatır!

Kavramsal Temeller:
* `struct`: Her şeyin kalbidir ama sıradan bir Cruct değildir. Ona `has key`, `has store`, `has drop` gibi Fiziksel Büyüler (Abilities) Basılır.
  - Eğer `has drop` demezseniz, o Obje hafızadan silinemez! Mutlaka birine devredilmesi gerekir. Bu sayede "Müşterinin parası kod içinde kayboldu" bug'ı YAŞANAMAZ.

### Örnek Bir Move Kodu: Kopyalanamaz ve Silinemez Bir Altın Sikke (Token) Tasarlamak
Aşağıda, Sadece tek bir Kişinin(Admin) yaratabildiği ve "Çoğaltılamayan / Kopyalanamayan" Efsanevi Move Resource (Para) Mimarisi:

```move
/* BU BIR MOVE (Aptos/Sui Akilil Kontrati) KODUDUR */

module BenimProjem::SikkeFabrikasi {
    
    // 1. BUYULU STRUCT (RESOURCE/Fiziksel Esya Kavrami)
    // Sadece 'store' (Sandikta/Cuzdanda Saklanabilir) becerisi verdik.
    // 'copy' Beceresi VERMEDIGIMIZ icin bu sikke KOPYALANIP COGALTILAMAZ! Hacker bunu yapamaz.
    // 'drop' Becerisi VERMEDIGIMIZ icin bu sikke Yere Atilip Yok Edilemez(Adam magdur edilemez)!
    struct AltinSikke has store {
        deger: u64
    }

    // 2. SIKKE YARATMA FONKSIYONU (Sadece Admin Basabilir)
    public fun darphane_bas(sikke_miktari: u64): AltinSikke {
        
        // Elimize(Hafizaya) Yepyeni bir Sikke Objesi (Resource) Geciyor
        let yeni_sikke = AltinSikke { deger: sikke_miktari };
        
        // Bu fiziksel Objeyi(return) Cagiran adama Kargo(Move) Et!
        return yeni_sikke;
    }

    // 3. IKI SIKKEYI BIRLESTIREREK TEK SIKKE YAPMA (Eritme Okeyi)
    // Fonskiyona 2 tane fiziksel Sikke objesi SOKULMAK ZORUNDA (Referans degil Vucut olarak gircek)!
    public fun sikkeleri_birlestir(sikke1: AltinSikke, sikke2: AltinSikke): AltinSikke {
        
        // O İki Sikkeyi Alip Parcalayalim (Destructuring Yapiyoruz, Icindeki Rakamiklari Al)
        // DİKKAT: Burada sikke1 ve sikke2 PARCALANDIGI ICIN EVRENDEN YOK OLUR!
        let AltinSikke { deger: deger1 } = sikke1;
        let AltinSikke { deger: deger2 } = sikke2;
        
        // Yepyeni Tek(1) Bir Sİkke yaratip degerlerini toplayalim Ve Adam VErcek
        return AltinSikke { deger: deger1 + deger2 };
    }
}
```
Yukarıdaki kod Solidity'de İnanılmaz zordur. Mesela bir kullanıcı `sikkeleri_birlestir` fonksiyonuna `sikke1`'i verdi ya... Eğer Fonksiyonun alt satırında Kullanıcı *Gidip Bir daha O (Sikke1)'i Harcamaya Çalışırsa*; Derleyici (Compiler) HATA Verir: **"Kardeş Sen Bu Sikkeyi Yukarıdaki Fonksiyona Move Ettin (Aktardın), Artık Sende Öyle Bir Şey Yok, Havadan Para Mı Basacaksın!"** diyerek Milyon dolarlık Hacki Daha siz Klavyedeyken Tıkar.

## Kimler Kullanır?
* Geleceğin Güvenli DeFi Mimarisini Yapan The Move-Ecosystem **Web3 Rust Mühendisleri**. 
* Şu An **Aptos (APT)** ve **Sui (SUI)** Adlı Devasa Ve Trilyonlarca İşlemci (TPS) Rekoru kıran Blokzincirlerin Yegane Dili oLarak Evrendeki Gücünü kanıtlamaktadır. Güvenliğin (Ve Kriptolojinin) Rust ile Evlenmesidir.
