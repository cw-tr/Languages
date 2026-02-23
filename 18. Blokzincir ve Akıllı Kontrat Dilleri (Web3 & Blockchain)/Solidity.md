# Solidity

## Özet
Solidity; 2014 yılında Ethereum'un kurucu ortakları (Gavin Wood ve Christian Reitwiessner başını çeker) tarafından icat edilen, yazım şekli JavaScript'e benzeyen ancak Statik ve Nesne-Yönelimli (Object-Oriented) olarak tasarlanmış, **Ethereum Sanal Makinesi (EVM - Ethereum Virtual Machine)** üzerinde çalışan "Akıllı Kontratları (Smart Contracts)" yazmak için kullanılan, Milyarlarca Dolarlık Kripto Paranın ve Merkeziyetsiz Finansın (DeFi) atardamarı olan **Web3 / Blokzincir Programlama Dilidir.**

## Nedir ve Ne İşe Yarar?
2009'da Bitcoin icat edildiğinde sadece "Para transferi (Ahmet'ten Mehmet'e 5 BTC yolla)" yapabiliyordu, üzerine bir bilgisayar programı (if-else mantığı) kuramıyordunuz.
2014'te Ethereum, "Bitcoin'in içine bir Bilgisayar katalım!" dedi. 
Eğer Ali, Mehmet'e "100 Dolar yollarsa ve Maç sonu Fenerbahçe Yenerse; O 100 Doları Bana İade et, Yenmezse adama Veb!" diyen bir İddaa (Bahis) sistemini; Hiçbir İnsan, Noter veya Banka olmadan MATEMATİKSEL KODA (Akıllı Kontrata) bağlama vizyonu ortaya çıktı. İşte o Büyülü Kod parçacığını (Akıllı Kontratı) yazan dil **Solidity**'dir.

**Ne İşe Yarar?**
* **DeFi (Merkeziyetsiz Finans - Uniswap / PancakeSwap):** Banka müdürü olmadan insanların Kredi Çektiği, Faize Para yatırdığı uygulamaların (DApps) kalbi Solidity'dir. Kod, parayı güvene alır.
* **NFT'ler (Değiştirilemez Tokenlar):** Dünyaca ünlü Milyon Dolarlık o Pikselli Maymun JPEG'lerinin (Bored Ape Yacht Club vb.) "Sahiplik Sertifikası", ERC-721 formatında bir Solidity kontratı tarafından mühürlenir.
* **DAO'lar (Merkeziyetsiz Otonom Organizasyonlar):** "Aşiret" gibi kodlanan şirketlerde; Toplanan Vergiler Yönetim Kuruluna(CEO) değil; Solidity kodundaki Hissedarların (Kripto cüzdanı sahiplerinin) Oy Çokluğu Koduna göre (If %51 EVET -> Parayı Yolla) Otomatik olarak Harcanır.

## Dilin Mantığı ve Kod Yapısı
Solidity Görsel olarak JavaScript ve C++'in Çocuğu gibidir. Ama Mimarisi Tıpkı Cihaz Geliştiren **Hard-Realtime (C/Rust)** Dilleri kadar Şizofreniktir:
Çünkü, Solidity'de yazdığınız her kodun Bilgisayarda çalışma **Maliyeti Vardır (Gas Fee)**! Eğer Siz array (döngü) içinde 1 Milyon defa Dönen bir kod yazarsanız; Kod Çalıştığında Ethereum Ağı (Miners/Madenciler) Kullanıcıdan YÜZLERCE DOLAR (Gerçek Para/Ether) Keser. Solidity programcısı, *Damlaya Kıyamayan Cimri Mimar* olmalıdır. Optimizasyon hayattır.

### Örnek Bir Solidity Kodu (Basit Bir Bağış/Kumbara Kontratı)
Açık kaynak olarak yayımlanan Ve İçine "Ethereum (Bakiye/Para)" kabul eden, Sadece Kontratı Yaratan Adamın İstediğinde Parayı Çekebildiği Kod:

```solidity
/* BU BIR SOLIDITY (Akilli Kontrat) KODUDUR */

// 1. LISANS VE DERLEYICI(COMPILER) VERSIYONU BELIRTME (Zorunlu Guvenlik)
// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

// 2. KONTRAT (SMART CONTRACT = Nesne/Class Mantigi)
contract GizliKumbara {
    
    // Durum Degiskenleri (Blockchain Ağına/Servera KAZINACAK ve Asla Silinmeyecek Veriler):
    address payable public owner;    // Kumbaranın Sahibi (Onun Cüzdan Adresi: 0x12..3Ac)
    uint256 public toplamBagis;      // Kumbaradaki Toplam Para Miktari (Unsigned Int)

    // 3. KURUCU (CONSTRUCTOR): Bu kod Agda(Internette) ilk yayınlandığında Sadece 1 Kere Calistir!
    constructor() {
        // Kontrati agda kim Yayinladiysa(msg.sender), KUMBARANIN SAHIBI O'dur!
        owner = payable(msg.sender); 
    }

    // 4. PARA YATIRMA FONKSIYONU (Payable Mührü Cok Kritik!)
    // 'payable' demek = Bu fonksiyon Çağrıldığında içine GERCEK ETHER (Kripto Para) Koyabilirim demek!
    function bagisYap() public payable {
        
        // Eger adam Fonksiyona 0'dan buyuk para Atmissa kabul et:
        require(msg.value > 0, "Sifir Ether Gonderemezsiniz Lutfen Bagis Yapin!");
        
        // Kumbaranin Istatistigini arttir (Gonderilen Miktari = msg.value) ekle:
        toplamBagis += msg.value;
    }

    // 5. PARAYI CEKME FONKSİYONU (Sadece Sahibi Cekebilir)
    function kumbarayi_Patlat() public {
        
        // GUVENLIK: Suan bu fonksiyonu cagiran Cüzdan(msg.sender), Sahibiyle(Owner) Ayni Kisi mi?! 
        // Degilse Hata firlat ve Geri Dönder (Hack girisimini Engelle!)
        require(msg.sender == owner, "Sen Patron Degilsin, Hirsizsin Parayi Cekemezsin!");
        
        // Kontratin Iclerinde biriken TUM PARAYI(address(this).balance) -> Sahibe (Owner'a) Transfer et!
        owner.transfer(address(this).balance);
        
        toplamBagis = 0; // Kasayi Sifirla
    }
}
```
Eğer siz Bu kodu yazara Ethereum ağına Yükler ve "KumbrayıPatlat()" tuşuna *Sahibi Olmadığınız Cüzdandan*(Örn Metamask'dan) Basarsanız; Kod Çalışmaz, İçindeki Parayı (DeFi havuzunu) sömüremezsiniz. Matematik adalettir. 

## Kimler Kullanır?
* Kripto Para piyasalarındaki **Web3 Geliştiricileri ve Akıllı Kontrat Mühendisleri (Smart Contract Auditors)**. Günümüzde Bankacılık sektöründen Kaçan Zeki yazılımcıların Dünyanın en fazla maaşı ve Risk primi (1 Harf Hata yaparsanız Hacklenir 10 Milyon dolar kaybedersiniz) ile çalıştığı Tehlikeli alandır.
* Ethereum'un yanı sıra EVM-Uyumlu (Avalanche, Binance Smart Chain, Polygon) gibi tüm ağlar yine Solidity kabul eder. Web3 Evreninin JavaScript'idir.
