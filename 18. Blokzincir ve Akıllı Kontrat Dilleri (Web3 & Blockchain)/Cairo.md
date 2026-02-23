# Cairo

## Özet
Cairo; 2020 yılında StarkWare Industries firması tarafından icat edilen, Turing-Tam (Turing Complete) olup yazdığınız bütün işlemlerin, algoritmaların ve hesaplamaların Sonucunu **"Sıfır-Bilgi İspatı (Zero-Knowledge Proof - ZK-STARKs)"** teknolojisiyle şifreleyerek Kanıtlayabilen; ve günümüzde Starknet (Ethereum Layer-2) Ağı üzerinde Dünyanın ilk şifrelenebilir(İspatlanabilir) Akıllı Kontratlarını ve Kuantum-Dirençli DApp'lerini oluşturmak için Geliştirilmiş, Rust-Sintakslı Matematik ve Kriptografi Dilidir.

## Nedir ve Ne İşe Yarar?
Ethereum (Ve Bitcoin) Harikadır ancak Bütün Dünya Sizin Bütün İşlemlerinizi Şeffafça Görür (Ali Mehmet'e 5 dolar attı). Ayrıca Eğer Ethereum Ağında "Pi Sayısının İlk 50000 Basamağını Hesapla" derseniz Ağ Felç olur ve Sizden Milyonlarca Dolar Fatura (Gas) Alır.

**STARKs ve CAIRO MUCİZESİ Şudur:**
Siz devasa İşlemi kendi Bilgisayarınızda veya StarkWare'in Sunucusunda (Off-Chain) Hızlıca ve Bedavaya Hesaplarsınız! Sonuç : `5`. Lakin Bütün Dünya Bilecek mi Sen hile Yaptın mı yapmadın mı?
İşte **Cairo Dili**, Sen O hesaplamayı Yaparken Arkadan Küçücük Matematiksel bir "Proof (Kanıt - O Zarfı Kapalıyken Çözdüğünü İspatlayan Formül)" yaratır. Siz Bu Ufacık Kanıtı(300 Byte) Ethereum Ağına Yollarsanız, Ethereum Sizin o 1 Ay süren Hesabın doğruluğunu (Kodun çalışmasını Tekrar Oynatmadan) 1 Milisaniyede Ve Cüzzi bir Ücretle Doğrular!!!

**Ne İşe Yarar?**
* **Rollups (Ethereum Ölçeklemesi - Layer 2):** 10.000 Kişinin Borsadaki Alım ve Satım (Trade) verisini Alır, Bilgisayarda tek bir Matematiksel ZK-Kanıt Yumağı (Cairo Proof) yapar. Onu Ana Ağa(Ethereum'a) gönderir. Eskiden Ethereum 10.000 İşlem Başına 50.000 Dolar alırken, Starknet'in Cairo Kanıtı sayesinde TOPLAM 1 Dolar'a bu devasa Yük Onaylatılır. İnterneti Hızlandırmanın Saf Matematiğidir.
* **Gizlilik Oyunları ve ZK-Gaming:** Satranç oynuyorsunuz (Onchain). Hamlenizi yaparsınız, Lakin Karşı taraf Hamlenizin Ne olduğunu Görmez (Zero-Knowledge), Sadece Matematiksel olarak "Hamlenin Kutsal Kurallara Uyduğunu" Onaylayan bir Cairo İspatı Görür (Açık Kartla Gizli Poker oynanır).

## Dilin Mantığı ve Kod Yapısı
Cairo ilk çıktığında (Cairo 0), C++ ve Assembly karışımı Saf Dehşet verici, okuması insan beynine aykırı CPU Sicil (Register) hafızaları kullanan Bir Kabustu.
Ancak **Cairo 1.0 (2023 Güncellemesi) ile Tamamen "Rust" Dilinin Klonunu (Sintaksını) aldılar**. Rust gibi Pattern Matching, Vektörler Ve Option/Result dönüşleri kullanılarak Geliştiricileri Rahatlattılar.

**Sihri Nerededir?**
Derleyicisi (Sierra adı verilen Bir Ara katman), Siz yazıyı yazarken "O Komutun Kriptografik Polinomlarını (Denklemini)" çıkartır. Yani sizin yazdığını `if-else` sadece bir kod değil, Evrensel bir Matematik Cebir denklemine dönüşür!

### Örnek Bir Cairo Kodu: Doğrulanabilir (İspat Çıkartan) Toplama Kontratı Makinesi
Starknet Ağı üzerine Yollanıp, İnsanların Toplam Matematiğini Kanıtla Yapabileceği (Rust Kokulu) Modern Cairo Kontratı:

```rust
// BU BIR CAIRO (Starknet ZK-Kontrat) KODUDUR (.cairo)

// 1. STARKNET OZELLIKLERI (Attribute)
// Derleyiciye bunun Bir Kontrat oldugunu Söylüyoruz!
#[starknet::interface]
trait IBasitKasa<TContractState> {
    // Kasa'nin Bakiyesini Ogrenme(View)
    fn bakiye_getir(self: @TContractState) -> u128;
    // Bakiye(Ekleme) Fonksiyonu - Bu Calistiginda Arkada Kripto İspat Uretilir
    fn bakiye_arttir(ref self: TContractState, miktar: u128);
}


#[starknet::contract]
mod BasitKasa {
    
    // 2. HAFIZA (Storage - Blockchain üzerinde Tutulacak Veriler)
    #[storage]
    struct Storage {
        bakiye: u128,  // Cok basit, Kasadaki para.
    }

    // 3. UYGULAMA(IMPLEMENTASYON) - Arayuzun İcerigini dolduralim
    #[abi(embed_v0)]
    impl BasitKasaImpl of super::IBasitKasa<ContractState> {
        
        // Sadece okuyacak Function (Self 'e referans atilir Mute edilmez)
        fn bakiye_getir(self: @ContractState) -> u128 {
            self.bakiye.read()
        }

        // DEGISIKLİk YAPACAK FONKSİYON (ref self - Yani Durumu Değiştirecek İzin var)
        fn bakiye_arttir(ref self: ContractState, miktar: u128) {
            
            // Su anki bakiyeyi Oku
            let simdiki_bakiye = self.bakiye.read();
            
            // Uzerine Ekleyip Mute Et Ve Storage'a YAZ(Write)! 
            // (Iste Tam bu islem Starknet Prover tarafindan Matematiksel Kanıta ZK-Kanıtına Dönüştürülür!)
            self.bakiye.write(simdiki_bakiye + miktar);
        }
    }
}
```
Mimarisi aynı Paslı-Cekçek (Rust) ve Solidity'nin birleşimine benzer. Ancak Starknet Prover (Derleyecinin Üstündeki Bulut Çekirdeği) bu `bakiye.write` için O kadar Ağır bir Polinom(Denklem) üretir ki, Kod Ethereum gibi milyonlarca makinede çalıştırılmaz. Sadece Starknet bunu Onaylar Kriptoyu atar Veee Ana Zincir Bunu Okur ve Sorgusuz kabul eder!

## Kimler Kullanır?
* Geleceğin Kriptoloji Çağında Bulunan (Yapay Zeka Ve Blockchain entegrasyonlarını kovalayan) **Sıfır-Bilgi (ZK) Mühendisleri ve Kriptografi Dikkate Alan Protokol Yazılımcıları.**
* Web2 ile Web3 arasına Dev Köprüler Kuran Kurumlar: (Örn: Bir Hastaneye Kanıt Vericeksiniz, Adama Tüm Raporlarınızı ve Kimliğinizi Açmak Yerine Cairo ile "Buyur Benim X Hastalığım Olmadığının Onaylı Matematiksel ZK-İspatı Budur!" dersiniz, Adam şifreyi göremez Ama Kodun Doğruluğunu Kabul etmek Zorundadır (Privacy Revolution). Kuantum sonrası Matematiğin Zirvesidir.
