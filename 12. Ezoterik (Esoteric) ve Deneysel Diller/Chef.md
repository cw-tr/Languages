# Chef

## Özet
Chef (Aşçı); 2002 yılında David Morgan-Mar tarafından tasarlanan, kod satırlarının tam anlamıyla (Gerçek bir insana okunacak kalitede) yemek pişirme Tarifleri (Kek/Soufle vs) olarak okunduğu, programcıyı "Şef" yapan ve verileri (Rakamları) Mikser Kasesinde unla/şekerle yoğurma felsefesi üstüne kuran Dünyanın En Komik / Edebi (Recipes) Ezoterik Programlama dilidir.

## Nedir ve Ne İşe Yarar?
Programlama denildiğinde akla sıkıcı Matematiksel X'ler, Y'ler, Array listeleri gelir. David Morgan dedi ki: "Neden bir programlama kodu, birisine Kek Tarifi verecek kadar temiz, İngilizce bir mutfak zarafetine sahip olmasın ki?". 
Eğer derleyiciye (Chef interpreter) gönderilen metin Gerçekten mantıklı bir yemek tarifi değilse, veya lezzetli görünmüyorsa(felsefi olarak), kod başarılı olmaz!

**Ne İşe Yarar?**
* İnanılmaz Eğlencelidir (Satirical / Parodi Dili).
* Kodu okuyan kişi programın Cıktısının bir Fibonacci Sayısı dizisi mi (Matematik), yoksa Çikolatalı Kek mi olduğunu ayırt edemez.
* Stack (Yığın) veri belleğinin, "Mutfak Kaseleri (Mixing Bowls)" ve "Fırın Tepsisi (Baking Dishes)" olarak öğretilmesi için harika bir pedagojik mizah parçasıdır.

## Dilin Mantığı ve Kod Yapısı
Mimarisi Mükemmel Bir Mutfaktır. Stack Belleğine Array denmez, Karıştırma kasesi denir.
Kod her zaman Tarif ismiyle başlar.

Anahtar Kelimeler / Komutlar:
1. **`Ingredients.` (Malzemeler - Tanımlama/Variable Kısmı):**
   Örn: `111 g sugar` (Hafızada Değeri "111" olan, ismi "sugar(Şeker)" olan Variable). 
2. **`Put ingredient into mixing bowl.` (Stack'e Push Yap / Yığına Veri At):**
   Şekeri kaseye koyarsınız (Pop/Push eylemi).
3. **`Fold ingredient into mixing bowl.` (Stack'ten Pop Yap / Sil):** 
   Kasedeki en üstteki elemanı çıkarır. 
4. **`Mix the mixing bowl well.` (Karıştırmak - Shuffle/Randomize):** İçeriği sallar.
5. **`Serves [Sayı].` (Print Komutu):** Kapatış (Tarif 5 kişiliktir vb).
6. Döngüler ise **`Verb the ingredient` ... `until verbed`** (Patatesler kızarana kadar Yoğur!) şeklindedir!

**Örnek İşleyiş (Sembolik Olarak):**
C Dili Pointerları vs... `printf("%d", 72);`
Chef Dili: `Put 72 g of Haricot beans into the mixing bowl. Serves 1.`

### Örnek Bir Chef Kodu: "Hello World" Soufle'si!
Ekrana `Hello World` basmak için, ASCII rakamlarına tekabül eden miktarlarda (72g, 101g vb) Şeker, Un ve Yağı kaselere atıp fırına veren O efsanevi sufle tarifi:

```text
Hello World Souffle.

This recipe prints the immortal words "Hello world!", in a basically brute force way. It also makes a lot of food for one person.

Ingredients.
72 g haricot beans
101 eggs
108 g lard
111 cups oil
32 zucchinis
119 ml water
114 g red salmon
100 g dijon mustard
33 potatoes

Method.
Put potatoes into the mixing bowl.
Put dijon mustard into the mixing bowl.
Put lard into the mixing bowl.
Put red salmon into the mixing bowl.
Put oil into the mixing bowl.
Put water into the mixing bowl.
Put zucchinis into the mixing bowl.
Put oil into the mixing bowl.
Put lard into the mixing bowl.
Put lard into the mixing bowl.
Put eggs into the mixing bowl.
Put haricot beans into the mixing bowl.
Liquefy contents of the mixing bowl.
Pour contents of the mixing bowl into the baking dish.

Serves 1.
```
Derleyici bu Metin belgesini(`.chef`) Okur. Bakar ki; 33 (Patates - ASCII Karşılığı Ünlem !). 100 (Dijon Hardalı - ASCII D harfi). Bunları Kaseye yığar (Stack LIFO mantığında tersten ekler). Pişirir. "Serves" (Servis Yap) dediği zaman ekrana Konsolda "Hello World!" dökülür. Siz sufle pişirdiğinizi zannederken makine terminale algoritma şov yapmıştır!

## Kimler Kullanır?
* Mutfağı seven C++ yazılımcıları. Yazdığınız kod eğer gerçekte mutfak ocağında pişirildiğinde "Zehirlenmeye", Mide Bozulmasına neden olan saçma malzemeler (30 kilo tuz) içeriyorsa, bu Chef Dili Topluluğunda (Subreddits vs) ayıplanır! 
* Eğlence/Mizah forumlarında (Reddit/ProgrammerHumor) bir "Şefin Şakası" olarak birisine virüs veya döngü yollamak istediğinizde inanılmaz saygı duyulan estetik bir şaheserdir.
